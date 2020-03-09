{-# LANGUAGE BlockArguments #-}
module Test.Data.JoinSemilattice.Class.Integral where

import Data.Holmes (IntegralR (..))
import Hedgehog

integralR_divModR :: (IntegralR x, Eq x, Integral x, Show x) => Gen x -> Property
integralR_divModR gen = property do
  b <- forAll gen
  c <- forAll gen
  d <- fmap (`mod` b) (forAll gen)
  let a = b * c + d

  let ( a', _, _, _ ) = divModR ( mempty, b, c, d )
  a' === b * c + d

  let ( _, b', _, _ ) = divModR ( a, mempty, c, d )
  b' === ((a - d) `div` c)

  let ( _, _, c', _ ) = divModR ( a, b, mempty, d )
  c' === a `div` b

  let ( _, _, _, d' ) = divModR ( a, b, c, mempty )
  d' === a `mod` b
