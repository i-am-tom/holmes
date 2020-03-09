{-# LANGUAGE BlockArguments #-}
module Test.Data.JoinSemilattice.Class.Fractional where

import Data.Holmes (FractionalR (..))
import Hedgehog

fractionalR_multiplyR :: (FractionalR x, Eq x, Fractional x, Show x) => Gen x -> Property
fractionalR_multiplyR gen = property do
  a <- forAll gen
  b <- forAll gen
  let c = a * b

  let ( _, _, c' ) = multiplyR ( a, b, mempty )
  c' === a * b

  let ( a', _, _ ) = multiplyR ( mempty, b, c )
  a' === c / b

  let ( _, b', _ ) = multiplyR ( a, mempty, c )
  b' === c / a
