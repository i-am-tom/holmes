{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

{-|
Module      : Control.Monad.Watson
Description : A much purer soul than Holmes.
Copyright   : (c) Tom Harding, 2020
License     : MIT

Watson works in a near-identical way to Holmes, but with one distinction: its
base type is 'ST' rather than 'IO', so the API calculates the results with
"observably pure" functions. There are downsides: for example, 'Watson' can't
perform random restart with operations like 'Control.Monad.Holmes.shuffle'.
However, this is often an acceptable compromise to avoid 'IO' entirely!
-}
module Control.Monad.Watson
  ( Watson
  , MonadCell (..)

  , backward
  , forward
  , runAll
  , runOne
  , satisfying
  , unsafeRead
  , whenever
  ) where

#if __GLASGOW_HASKELL__ == 806
import Control.Monad.Fail (MonadFail, fail)
#endif
import Control.Monad.ST (ST, runST)
import Control.Monad.Cell.Class (MonadCell (..))
import qualified Control.Monad.Cell.Class as Cell
import Control.Monad.MoriarT (MoriarT (..))
import qualified Control.Monad.MoriarT as MoriarT
import Data.Coerce (coerce)
import Data.Input.Config (Config (..))
import Data.JoinSemilattice.Class.Eq (EqR)
import Data.JoinSemilattice.Class.Merge (Merge)
import Data.Kind (Type)
import Data.Propagator (Prop)
import qualified Data.Propagator as Prop
import Type.Reflection (Typeable)

-- | A monad capable of solving constraint problems using 'ST' as the
-- evaluation type. Cells are represented using 'Data.STRef.STRef' references,
-- and __provenance__ is tracked to optimise backtracking search across
-- multiple branches.
newtype Watson (h :: Type) (x :: Type)
  = Watson { runWatson :: MoriarT (ST h) x }
  deriving (Functor, Applicative, Monad)

instance MonadFail (Watson h) where
  fail _ = discard

instance MonadCell (Watson h) where
  newtype Cell (Watson h) x = Cell { unwrap :: Cell (MoriarT (ST h)) x }

  discard = coerce (discard @(MoriarT (ST h)))
  fill = fmap Cell . coerce (fill @(MoriarT (ST h)))

  watch (Cell cell) = coerce (watch @(MoriarT (ST h)) cell)
  with  (Cell cell) = coerce (with  @(MoriarT (ST h)) cell)
  write (Cell cell) = coerce (write @(MoriarT (ST h)) cell)

-- | Unsafely read from a cell. This operation is unsafe because it doesn't
-- factor this cell into the provenance of any subsequent writes. If this value
-- ends up causing a contradiction, we may end up removing branches of the
-- search tree that are totally valid! This operation is safe as long as it is
-- the __very last thing__ you do in a computation, and its value is __never__
-- used to influence any writes in any way.
unsafeRead :: Cell (Watson h) x -> Watson h x
unsafeRead = coerce . MoriarT.unsafeRead . unwrap

-- | Run a function between propagators "backwards", writing the given value as
-- the output and then trying to push information backwards to the input cell.
backward :: (Typeable x, Merge x, Merge y) => (forall m. MonadCell m => Prop m x -> Prop m y) -> y -> Maybe x
backward f y = runST $ MoriarT.runOne $ runWatson do
  input  <- Cell.make
  output <- Prop.down (f (Prop.up input))

  Cell.write output y
  unsafeRead input

-- | Run a function between propagators with a raw value, writing the given
-- value to the "input" cell and reading the result from the "output" cell.
forward :: (Typeable x, Merge x, Merge y) => (forall m. MonadCell m => Prop m x -> Prop m y) -> x -> Maybe y
forward f x = runST $ MoriarT.runOne $ runWatson do
  input  <- Cell.make
  output <- Prop.down (f (Prop.up input))

  Cell.write input x
  unsafeRead output

-- | Interpret a 'Watson' program, returning a list of all successful branches'
-- outputs. It's unlikely that you want to call this directly, though;
-- typically, 'satisfying' or 'whenever' are more likely the things you want.
runAll :: (forall h. Watson h x) -> [ x ]
runAll xs = runST (MoriarT.runAll (runWatson xs))

-- | Interpret a 'Watson' program, returning the first successful branch's
-- result /if/ any branch succeeds. It's unlikely that you want to call this
-- directly, though; typically, 'satisfying' or 'whenever' are more likely the
-- things you want.
runOne :: (forall h. Watson h x) -> Maybe x
runOne xs = runST (MoriarT.runOne (runWatson xs))

-- | Given an input configuration, and a predicate on those input variables,
-- return the __first__ configuration that satisfies the predicate.
satisfying :: (EqR x b, Typeable x) => (forall h. Config (Watson h) x) -> (forall m. MonadCell m => [ Prop m x ] -> Prop m b) -> Maybe [ x ]
satisfying config f = runST (MoriarT.runOne (MoriarT.solve (coerce config) f))

-- | Given an input configuration, and a predicate on those input variables,
-- return __all configurations__ that satisfy the predicate. It should be noted
-- that there's nothing lazy about this; if your problem has a lot of
-- solutions, or your search space is very big, you'll be waiting a long time!
whenever :: (EqR x b, Typeable x) => (forall h. Config (Watson h) x) -> (forall m. MonadCell m => [ Prop m x ] -> Prop m b) -> [[ x ]]
whenever config f = runST (MoriarT.runAll (MoriarT.solve (coerce config) f))
