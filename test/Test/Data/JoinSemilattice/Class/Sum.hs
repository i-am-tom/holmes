{-# LANGUAGE BlockArguments #-}
module Test.Data.JoinSemilattice.Class.Sum where

import Data.Holmes (SumR (..))
import Hedgehog

sumR_addR :: (SumR x, Eq x, Num x, Show x) => Gen x -> Property
sumR_addR gen = property do
  a <- forAll gen
  b <- forAll gen
  let c = a + b

  let ( _, _, c' ) = addR ( a, b, mempty )
  c' === a + b

  let ( a', _, _ ) = addR ( mempty, b, c )
  a' === c - b

  let ( _, b', _ ) = addR ( a, mempty, c )
  b' === c - a
