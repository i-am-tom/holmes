{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Control.Monad.Cell.Class
Description : An interface for the primitive cell operations in a propagator network.
Copyright   : (c) Tom Harding, 2020
License     : MIT

/Are you just trying to use the library?/ If so, the contents of this module
shouldn't matter to you, so feel free to head straight over to the main
"Data.Holmes" module instead!

A __cell__ is the unit of storage in a propagator network. We can think of it
as "a description of a value", which is /refined/ over the course of a
computation.  Because we're functional programmers, the /described/ value is
__referentially transparent__ and __pure__: a cell's description must always be
of the /same/ value, and it can't change during the course of a computation.

Instead of __functions__ from one cell to another, we should try to think about
__relationships__ between cells. Addition, for example, could be seen as a
/function/ with two inputs, but it could also be seen as a /relationship/
between three values: the two components and their sum. The reason why this
helps us is that we might very well, for whatever reason, learn the sum
/before/ we learn both of the inputs. In these cases, it's useful to allow
information to flow in __multiple direcitons__. Why restrict ourselves to the
one-way flow of input-to-output when we can happily re-arrange equations on
paper?

Once we've built up our vocabulary for relationships, we just need a way to
lift them over cells. Intuitively, we should think of all relationships as
__invariants__. As cells' values are refined, these relationships are
constantly re-evaluated, and any new information can be spread around the
network to trigger, we hope, /more/ learnings that bring us closer to a
solution.

The 'Control.Monad.MoriarT.MoriarT' type provides a good reference
implementation for this interface, so head over there to see how we can use the
class to implement ideas like __provenance__ and __backtracking__.
-}
module Control.Monad.Cell.Class where

import Data.JoinSemilattice.Class.Merge (Merge)
import Data.Kind (Type)
import Data.Tuple (swap)
import Prelude

-- | The DSL for network construction primitives. The following interface
-- provides the building blocks upon which the rest of the library is
-- constructed.
--
-- If you are looking to implement the class yourself, you should note the lack
-- of functionality for ambiguity/searching. This is deliberate: for
-- backtracking search (as opposed to truth maintenance-based approaches), the
-- ability to create computation branches dynamically makes it much harder to
-- establish a reliable mechanism for tracking the effects of these choices.
--
-- For example: the approach used in the 'Control.Monad.MoriarT.MoriarT'
-- implementation is to separate the introduction of ambiguity into one
-- definite, explicit step, and all parameters must be declared ahead of time
-- so that they can be assigned indices. Other implementations should feel free
-- to take other approaches, but these will be implementation-specific.
class Monad m => MonadCell (m :: Type -> Type) where

  -- | The type of cells for this particular implementation. Typically, it's
  -- some sort of mutable reference ('Data.STRef.STRef', 'Data.IORef.IORef', or
  -- similar), but the implementation may attach further metadata to the
  -- individual cells.
  data Cell m :: Type -> Type

  -- | Mark the current computation as __failed__. For more advanced
  -- implementations that utilise backtracking and branching, this is an
  -- indication that we should begin a different branch of the search.
  -- Otherwise, the computation should simply fail without a result.
  discard :: m x

  -- | Create a new cell with the given value. Although this value's type has
  -- no constraints, it will be immutable unless it also implements 'Merge',
  -- which exists to enforce __monotonic__ updates.
  fill :: x -> m (Cell m x)

  -- | Create a callback that is fired whenever the value in a given cell is
  -- updated. Typically, this callback will involve potential writes to /other/
  -- cells based on the current value of the given cell. If such a write
  -- occurs, we say that we have __propagated__ information from the first cell
  -- to the next.
  watch :: Cell m x -> (x -> m ()) -> m ()

  -- | Execute a callback with the current value of a cell. Unlike 'watch',
  -- this will only fire once, and subsequent changes to the cell should /not/
  -- re-trigger this callback. This callback should therefore not be
  -- "registered" on any cell.
  with :: Cell m x -> (x -> m ()) -> m ()

  -- | Write an __update__ to a cell. This update should be merged into the
  -- current value using the '(Data.JoinSemilattice.Merge.<<-)' operation,
  -- which should behave the same way as '(<>)' for commutative and idempotent
  -- monoids. This therefore preserves the monotonic behaviour: updates can
  -- only __refine__ a value. The result of a 'write' must be /more refined/
  -- than the value before, with no exception.
  write :: Merge x => Cell m x -> x -> m ()

-- | In our regular Haskell coding, a binary function usually looks something
-- like @x -> y -> z@. When we view it as a /relationship/, we see that it's
-- actually a relationship between __three__ values: @x@, @y@, and @z@.
--
-- Given a function that takes everything we /currently/ know about these three
-- values, and returns three "updates" based on what each can learn from the
-- others, we can lift our three-way relationship (which, again, we can intuit
-- as a multi-directional binary function) into a network as a three-way
-- __propagator__. As an illustrative example, we might convert the '(+)'
-- function into something like:
--
-- @
-- addR :: (Int, Int, Int) -> (Int, Int, Int)
-- addR ( a, b, c ) = ( c - b, c - a, a + b )
-- @
--
-- In /practice/, these values must be 'Merge' values (unlike 'Int'), and so
-- any of them /could/ be 'mempty', or less-than-well-defined. This function
-- will take the three results as __updates__, and 'Merge' it into the cell,
-- so they will only make a difference /if/ we've learnt something new.
binary :: (MonadCell m, Merge x, Merge y, Merge z) => ((x, y, z) -> (x, y, z)) -> Cell m x -> Cell m y -> Cell m z -> m ()
binary f xs ys zs = do
  let update x y z = do
        let ( x', y', z' ) = f ( x, y, z )

        write xs x'
        write ys y'
        write zs z'

  watch xs \x -> with ys \y -> with zs \z -> update x y z
  watch ys \y -> with xs \x -> with zs \z -> update x y z
  watch zs \z -> with ys \y -> with xs \x -> update x y z

-- | Create a cell with "no information", which we represent as 'mempty'. When
-- we evaluate propagator computations written with the 'Data.Propagator.Prop'
-- abstraction, this function is used to create the result cells at each node
-- of the computation.
--
-- It's therefore important that your 'mempty' value is reasonably efficient to
-- compute, as larger computations may involve producing many of these values
-- as intermediaries. An 'Data.JoinSemilattice.Intersect.Intersect' of all
-- 'Int' values, for example, is going to make things run /very/ slowly.
make :: (MonadCell m, Monoid x) => m (Cell m x) 
make = fill mempty

-- | This function takes two cells, and establishes propagators between them in
-- both directions. These propagators simply copy across any updates that
-- either cell receives, which means that the two cells end up holding exactly
-- the same value at all times.
--
-- After calling this function, the two cells are entirely indistinguishable,
-- as they will always be equivalent. We can intuit this function as "merging
-- two cells into one".
unify :: (MonadCell m, Merge x) => Cell m x -> Cell m x -> m ()
unify = unary swap

-- | A standard unary function goes from an input value to an output value.
-- However, in the world of propagators, it is more powerful to rethink this as
-- a /relationship/ between two values.
--
-- A good example is the 'negate' function. It doesn't matter whether you know
-- the input or the output; it's always possible to figure out the one you're
-- missing. Why, then, should our program only run in one direction? We could
-- rephrase 'negate' from 'Int -> Int' to something more like:
--
-- @
-- negateR :: ( Maybe Int, Maybe Int ) -> ( Maybe Int, Maybe Int )
-- negateR ( x, y ) = ( x <|> fmap negate y, y <|> fmap negate x )
-- @
--
-- Now, if we're missing /one/ of the values, we can calculate it using the
-- other! This, and the 'binary' function's description above, give us an idea
-- of how functions and relationships differ. The 'unary' function simply lifts
-- one of these expressions into a two-way propagator between two cells.
--
-- The 'Merge' constraint means that we can use 'mempty' to represent "knowing
-- nothing" rather than the 'Maybe' in the above example, which makes this
-- function a little more generalised.
unary :: (MonadCell m, Merge x, Merge y) => ((x, y) -> (x, y)) -> Cell m x -> Cell m y -> m ()
unary f xs ys = do
  let update x y = do
        let ( x', y' ) = f ( x, y )
        write xs x' *> write ys y'

  watch xs \x -> with ys \y -> update x y
  watch ys \y -> with xs \x -> update x y
