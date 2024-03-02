{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Test.Data.Input.Config where

import Control.Monad.Holmes (Holmes, runOne)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Hashable (Hashable)
import Data.Input.Config (Input (..), Config (..), permute)
import qualified Data.Set as Set
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

possibilities :: Gen [Int]
possibilities = do
  let values = Gen.int (Range.linear 0 100)
  set <- Gen.set (Range.linear 1 5) values

  pure (Set.toList set)

from_fill :: forall x. (Eq x, Hashable x, Input x, Show x, Raw x ~ Int) => Property
from_fill = property do
  count  <- forAll (Gen.int (Range.linear 1 10))
  inputs <- forAll possibilities

  let config :: Config Holmes x
      config = count `from` inputs
  annotateShow (initial config)

  Just permutations <- liftIO (runOne (permute config))
  annotateShow permutations

  length permutations === length inputs ^ count
