{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Test.Data.JoinSemilattice.Class.Eq where

import Data.Holmes (BooleanR (..), EqR (..), neR)
import Hedgehog

eqR_eqR
  :: ( EqC f x
     , EqR f
     , Eq (f x)
     , Eq (f Bool)
     , Show (f x)
     , Show (f Bool)
     )
  => Gen (f x)
  -> Property
eqR_eqR gen = property do
  a <- forAll gen
  b <- forAll gen

  let ( _, _, c ) = eqR ( a, b, mempty )
  annotateShow c

  let ( a', _, _ ) = eqR ( mempty, b, c )
  annotateShow a'
  a' <> a === a

  let ( _, b', _ ) = eqR ( a, mempty, c )
  annotateShow b'
  b' <> b === b

eqR_negation
  :: ( EqC f x
     , EqR f
     , Eq (f x)
     , Eq (f Bool)
     , Show (f x)
     , Show (f Bool)
     )
  => Gen (f x)
  -> Property
eqR_negation gen = property do
  a <- forAll gen
  b <- forAll gen

  let ( _, _, c ) = eqR ( a, b, mempty )
  annotateShow c

  let ( _, _, d ) = neR ( a, b, mempty )
  annotateShow d

  let ( _, c' ) = notR ( c, mempty )
  c' === d

eqR_reflexivity
  :: ( EqC f x
     , EqR f
     , Eq (f x)
     , Eq (f Bool)
     , Show (f x)
     , Show (f Bool)
     )
  => Gen (f x)
  -> Property
eqR_reflexivity gen = property do
  a <- forAll gen

  let ( _, _, c ) = eqR ( a, a, mempty )
  c <> trueR === trueR

eqR_symmetry
  :: ( EqC f x
     , EqR f
     , Eq (f x)
     , Eq (f Bool)
     , Show (f x)
     , Show (f Bool)
     )
  => Gen (f x)
  -> Property
eqR_symmetry gen = property do
  a <- forAll gen
  b <- forAll gen

  let ( _, _, x ) = eqR ( a, b, mempty )
      ( _, _, y ) = eqR ( b, a, mempty )

  x === y
