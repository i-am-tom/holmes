{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}

{-|
Module      : Control.Monad.Watson
Description : Performant join semilattice-based knowledge-merging.
Copyright   : (c) Tom Harding, 2020
License     : MIT

= Join semilattices

A __join semilattice__ is a 'Monoid' with two extra laws:

prop> x <> y === y <> x -- Commutativity
prop> x <> x === x      -- Idempotence

Within every cell, we store a join semilattice, and all writes are added into
the cell using '(<>)'. Adding the above laws introduces enough structure to
ensure that all functions between cells are __monotonic__. In other words, if
we assume that @x@ "implies" @y@ if @x <> y === x@, the value /after/ a write
will always imply the value /before/.

We can therefore see each value as "moving up" some chain towards the final
answer. More interestingly, the final answer implies /every value/ that has
ever been in the cell. I like to use the intuition of __knowledge__ for join
semilattices:

- 'mempty' represents "knowing nothing" about a value.
- '(<>)' is a function that /combines/ two knowledge bases into one.
- @x@ implies @y@ if @y@ tells us nothing that @x@ doesn't already tell us.

When we think about pure functions and referential transparency, we tend to say
that "the value of a variable never changes". In the language of propagator
networks, we can tweak this a little to say, "the value /being described/ by a
cell's knowledge never changes".

= Merging

In a naïve system, we could simply define the join semilattice class as
follows:

@
class Monoid x => JoinSemilattice (x :: Type)
@

(It would need no methods as it's really just some extra assertions on '(<>)').
This would be fine, but there are a few shortcomings when we come to implement
our 'Control.Monad.Cell.Class.write' operation:

- We don't want to trigger propagators if we don't need to, so we'd want to
  check whether the result is different to the value that was there before.
  We'd most likely do this with a standard '(==)' comparison, but this could be
  quite expensive!

- We don't have a notion of "failure state", so we don't know when we can
  discard branches. If we don't know when to /discard/ branches, we either have
  to implement assertions elsewhere (which puts more work onto the user) /or/
  discard nothing (which makes many problems intractably slow to compute).

The cleanest solution I could find to this is expressed in the 'Result' type,
which allows the type simultaneously to compute the merge result /and/ the
resulting effect on the cell or network. In theory, it should respect the
'(<>)' operation's behaviour, but with the added 'Failure' state. Not every
type /needs/ to have a 'Failure' state, but it means that the user needn't
write their own assertion boilerplate for the usual suspects (such as the
'Data.JoinSemilattice.Defined.Conflict' constructor).

-}
module Data.JoinSemilattice.Class.Merge where

import Data.Hashable (Hashable)
import Data.JoinSemilattice.Defined (Defined (..))
import Data.JoinSemilattice.Intersect (Intersect (..))
import qualified Data.JoinSemilattice.Intersect as Intersect
import Data.Kind (Type)

-- | The result of merging some news into a cell's current knowledge.
data Result (x :: Type)
  = Unchanged -- ^ We've learnt nothing; no updates elsewhere are needed.
  | Changed x -- ^ We've learnt something; fire the propagators!
  | Failure   -- ^ We've hit a failure state; discard the computation.
  deriving stock (Eq, Functor, Ord, Show)

instance Semigroup x => Semigroup (Result x) where
  Changed x <> Changed y = Changed (x <> y)

  Failure <> _ = Failure
  _ <> Failure = Failure

  Unchanged <> y = y
  x <> Unchanged = x

instance Semigroup x => Monoid (Result x) where
  mempty = Unchanged

instance Applicative Result where
  pure = Changed

  Failure <*> _ = Failure
  _ <*> Failure = Failure

  Unchanged <*> _ = Unchanged
  _ <*> Unchanged = Unchanged

  Changed f <*> Changed x = Changed (f x)

-- | Join semilattice '(<>)' specialised for propagator network needs. Allows
-- types to implement the notion of "knowledge combination".
class Monoid x => Merge (x :: Type) where

  -- | Merge the news (right) into the current value (left), returning an
  -- instruction on how to update the network.
  (<<-) :: x -> x -> Result x

instance Eq content => Merge (Defined content) where
  Conflict <<- _ = Failure
  _ <<- Conflict = Failure

  _       <<- Unknown = Unchanged
  Unknown <<- that    = Changed that

  Exactly this <<- Exactly that
    | this == that = Unchanged
    | otherwise    = Failure

instance (Bounded x, Enum x, Ord x, Hashable x)
    => Merge (Intersect x) where
  before <<- news = case before <> news of
    joined | Intersect.size joined < 1                     -> Failure
           | Intersect.size joined < Intersect.size before -> Changed joined
           | otherwise                                     -> Unchanged

