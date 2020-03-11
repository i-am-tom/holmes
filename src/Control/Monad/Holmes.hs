{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module      : Control.Monad.Holmes
Description : A monad for constructing constraint-solving computations, and executing them inside 'IO'.
Copyright   : (c) Tom Harding, 2020
License     : MIT

'Holmes' is a type for solving constraint problems. These computations are
executed with 'IO', which allows for extra features such as the ability to
'shuffle' the input configuration.

If this isn't a feature you require, you may prefer to use the
"Control.Monad.Watson" interface, which offers a pure version of the API thanks
to its use of 'Control.Monad.ST'. The internal code is shared between the two,
so results between the two are consistent.
-}
module Control.Monad.Holmes
  ( Holmes
  , MonadCell

  , backward
  , forward
  , runAll
  , runOne
  , satisfying
  , shuffle
  , unsafeRead
  , whenever
  ) where

import Control.Monad.Cell.Class (MonadCell (..))
import Control.Monad.IO.Class (MonadIO (..))
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
import qualified Hedgehog.Gen as Gen
import Type.Reflection (Typeable)

-- | A monad capable of solving constraint problems using 'IO' as the
-- evaluation type. Cells are represented using 'Data.IORef.IORef' references,
-- and __provenance__ is tracked to optimise backtracking search across
-- multiple branches.
newtype Holmes (x :: Type)
  = Holmes { runHolmes :: MoriarT IO x }
  deriving (Functor, Applicative, Monad)

instance MonadFail Holmes where
  fail _ = discard

instance MonadCell Holmes where
  newtype Cell Holmes x = Cell { unwrap :: Cell (MoriarT IO) x }

  discard = coerce (discard @(MoriarT IO))
  fill = fmap Cell . coerce (fill @(MoriarT IO))

  watch (Cell cell) = coerce (watch @(MoriarT IO) cell)
  with  (Cell cell) = coerce (with  @(MoriarT IO) cell)
  write (Cell cell) = coerce (write @(MoriarT IO) cell)

-- | Unsafely read from a cell. This operation is unsafe because it doesn't
-- factor this cell into the provenance of any subsequent writes. If this value
-- ends up causing a contradiction, we may end up removing branches of the
-- search tree that are totally valid! This operation is safe as long as it is
-- the __very last thing__ you do in a computation, and its value is __never__
-- used to influence any writes in any way.
unsafeRead :: Cell Holmes x -> Holmes x
unsafeRead = coerce . MoriarT.unsafeRead . unwrap

-- | Run a function between propagators "backwards", writing the given value as
-- the output and then trying to push information backwards to the input cell.
backward :: (Typeable x, Merge x, Merge y) => (forall m. MonadCell m => Prop m x -> Prop m y) -> y -> IO (Maybe x)
backward f y = MoriarT.runOne $ runHolmes do
  input  <- Cell.make
  output <- Prop.down (f (Prop.up input))

  Cell.write output y
  unsafeRead input

-- | Run a function between propagators with a raw value, writing the given
-- value to the "input" cell and reading the result from the "output" cell.
forward :: (Typeable x, Merge x, Merge y) => (forall m. MonadCell m => Prop m x -> Prop m y) -> x -> IO (Maybe y)
forward f x = MoriarT.runOne $ runHolmes do
  input  <- Cell.make
  output <- Prop.down (f (Prop.up input))

  Cell.write input x
  unsafeRead output

-- | Interpret a 'Holmes' program into 'IO', returning a list of all successful
-- branches' outputs. It's unlikely that you want to call this directly,
-- though; typically, 'satisfying' or 'whenever' are more likely the things you
-- want.
runAll :: Holmes x -> IO [ x ]
runAll = coerce (MoriarT.runAll @IO)

-- | Interpret a 'Holmes' program into 'IO', returning the first successful
-- branch's result /if/ any branch succeeds. It's unlikely that you want to
-- call this directly, though; typically, 'satisfying' or 'whenever' are more
-- likely the things you want.
runOne :: Holmes x -> IO (Maybe x)
runOne = coerce (MoriarT.runOne @IO)

-- | Given an input configuration, and a predicate on those input variables,
-- return the __first__ configuration that satisfies the predicate.
satisfying :: (EqR x b, Typeable x) => Config Holmes x -> (forall m. MonadCell m => [ Prop m x ] -> Prop m b) -> IO (Maybe [ x ])
satisfying (coerce -> config :: Config (MoriarT IO) x) f = MoriarT.runOne (MoriarT.solve config f)

-- | Shuffle the refinements in a configuration. If we make a configuration
-- like @100 `from` [1 .. 10]@, the first configuration will be one hundred @1@
-- values. Sometimes, we might find we get to a first solution /faster/ by
-- randomising the order in which refinements are given. This is similar to the
-- "random restart" strategy in hill-climbing problems.
--
-- Another nice use for this function is procedural generation: often, your
-- results will look more "natural" if you introduce an element of randomness.
shuffle :: Config Holmes x -> Config Holmes x
shuffle Config{..} = Config initial \x -> do
  let shuffle' = liftIO . Gen.sample . Gen.shuffle
  Holmes (runHolmes (refine x) >>= shuffle')

-- | Given an input configuration, and a predicate on those input variables,
-- return __all configurations__ that satisfy the predicate. It should be noted
-- that there's nothing lazy about this; if your problem has a lot of
-- solutions, or your search space is very big, you'll be waiting a long time!
whenever :: (EqR x b, Typeable x) => Config Holmes x -> (forall m. MonadCell m => [ Prop m x ] -> Prop m b) -> IO [[ x ]]
whenever (coerce -> config :: Config (MoriarT IO) x) f = MoriarT.runAll (MoriarT.solve config f)
