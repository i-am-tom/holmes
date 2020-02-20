{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Test.Data.JoinSemilattice.Class.Ord where

import Data.Holmes (BooleanR (..), OrdR (..))
import Hedgehog

ordR_lteR :: (OrdR x b, Show b, Eq x, Show x) => Gen x -> Property
ordR_lteR gen = property do
  a <- forAll gen
  b <- forAll gen

  let ( _, _, c ) = lteR ( a, b, mempty )
  annotateShow c

  let ( a', _, _ ) = lteR ( mempty, b, c )
  annotateShow a'
  a' <> a === a

  let ( _, b', _ ) = lteR ( a, mempty, c )
  annotateShow b'
  b' <> b === b

ordR_symmetry :: (OrdR x b, Eq b, Eq x, Show x) => Gen x -> Property
ordR_symmetry gen = property do
  a <- forAll gen
  b <- forAll gen

  let ( _, _, x ) = lteR ( a, b, mempty )
      ( _, _, y ) = lteR ( b, a, mempty )

  if x == trueR && y == trueR
    then a === b
    else success
