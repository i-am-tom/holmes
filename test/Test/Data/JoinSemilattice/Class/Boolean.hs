{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Test.Data.JoinSemilattice.Class.Boolean where

import Data.Holmes (BooleanR (..))
import Hedgehog
import qualified Hedgehog.Gen as Gen

bool :: BooleanR x => Gen x
bool = Gen.element [ trueR, falseR ]

booleanR_andR :: forall x. (BooleanR x, Eq x, Show x) => Property
booleanR_andR = property do
  (a :: x) <- forAll bool
  (b :: x) <- forAll bool

  let ( _, _, c ) = andR ( a, b, mempty )
  annotateShow c

  let ( a', _, _ ) = andR ( mempty, b, c )
  annotateShow a'
  a' <> a === a

  let ( _, b', _ ) = andR ( a, mempty, c )
  annotateShow b'
  b' <> b === b

booleanR_deMorgan_and :: forall x. (BooleanR x, Eq x, Show x) => Property
booleanR_deMorgan_and = property do
  (a :: x) <- forAll bool
  (b :: x) <- forAll bool

  let ( _, _, c ) = andR ( a, b, mempty )
      ( _, a' ) = notR ( a, mempty )
      ( _, b' ) = notR ( b, mempty )
      ( _, _, d' ) = orR ( a', b', mempty )
      ( _, d ) = notR ( d', mempty )

  c === d

booleanR_deMorgan_or :: forall x. (BooleanR x, Eq x, Show x) => Property
booleanR_deMorgan_or = property do
  (a :: x) <- forAll bool
  (b :: x) <- forAll bool

  let ( _, _, c ) = orR ( a, b, mempty )
      ( _, a' ) = notR ( a, mempty )
      ( _, b' ) = notR ( b, mempty )
      ( _, _, d' ) = andR ( a', b', mempty )
      ( _, d ) = notR ( d', mempty )

  c === d

booleanR_orR :: forall x. (BooleanR x, Eq x, Show x) => Property
booleanR_orR = property do
  (a :: x) <- forAll bool
  (b :: x) <- forAll bool

  let ( _, _, c ) = orR ( a, b, mempty )
  annotateShow c

  let ( a', _, _ ) = orR ( mempty, b, c )
  annotateShow a'
  a' <> a === a

  let ( _, b', _ ) = orR ( a, mempty, c )
  annotateShow b'
  b' <> b === b

booleanR_notR :: forall x. (BooleanR x, Eq x, Show x) => Property
booleanR_notR = property do
  (a :: x) <- forAll bool

  let ( _, b ) = notR ( a, mempty )
  annotateShow b

  let ( a', _ ) = notR ( mempty, b )
  annotateShow a'
  a' === a
