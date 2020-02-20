{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Test.Data.JoinSemilattice.Defined where

import Control.Applicative (liftA2)
import Data.Holmes (AbsR (..), BooleanR (..), EqR (..), neR, OrdR (..), gteR, gtR, lteR, ltR, SumR (..), negateR, subR)
import Data.JoinSemilattice.Defined (Defined (..))
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Test.Data.Input.Config as Input
import qualified Test.Data.JoinSemilattice.Class.Abs as AbsR
import qualified Test.Data.JoinSemilattice.Class.Boolean as BooleanR
import qualified Test.Data.JoinSemilattice.Class.Eq as EqR
import qualified Test.Data.JoinSemilattice.Class.Ord as OrdR
import qualified Test.Data.JoinSemilattice.Class.Sum as SumR
import Test.Tasty.Hspec (Spec, it, shouldBe)
import qualified Test.Util.Laws as Laws

defined_int :: Gen (Defined Int)
defined_int = do
  content <- Gen.int (Range.linear 0 100)
  Gen.element [ Unknown, Exactly content, Conflict ]

hprop_from_fill :: Property
hprop_from_fill = Input.from_fill @(Defined Int)

hprop_semigroup_associativity :: Property
hprop_semigroup_associativity = Laws.semigroup_associativity defined_int

hprop_monoid_identity :: Property
hprop_monoid_identity = Laws.monoid_identity defined_int

hprop_join_semilattice_commutativity :: Property
hprop_join_semilattice_commutativity = Laws.semigroup_commutativity defined_int

hprop_join_semilattice_idempotence :: Property
hprop_join_semilattice_idempotence = Laws.semigroup_idempotence defined_int

hprop_functor_identity :: Property
hprop_functor_identity = Laws.functor_identity defined_int

hprop_functor_composition :: Property
hprop_functor_composition = Laws.functor_composition defined_int

hprop_applicative_identity :: Property
hprop_applicative_identity = Laws.applicative_identity defined_int

hprop_applicative_composition :: Property
hprop_applicative_composition = Laws.applicative_composition defined_int

hprop_applicative_homomorphism :: Property
hprop_applicative_homomorphism = Laws.applicative_homomorphism @Defined

hprop_applicative_interchange :: Property
hprop_applicative_interchange = Laws.applicative_interchange defined_int

hprop_absR :: Property
hprop_absR = AbsR.absR_absR defined_int

hprop_booleanR_andR_simple :: Property
hprop_booleanR_andR_simple = property do
  x <- forAll Gen.bool
  y <- forAll Gen.bool

  let ( _, _, z ) = andR ( Exactly x, Exactly y, mempty )
  z === Exactly (x && y)

hprop_booleanR_andR :: Property
hprop_booleanR_andR = BooleanR.booleanR_andR @(Defined Bool)

hprop_booleanR_notR :: Property
hprop_booleanR_notR = BooleanR.booleanR_notR @(Defined Bool)

hprop_booleanR_orR :: Property
hprop_booleanR_orR = BooleanR.booleanR_orR @(Defined Bool)

hprop_eqR_eqR_simple :: Property
hprop_eqR_eqR_simple = property do
  (Exactly -> x) <- forAll (Gen.int (Range.linear 0 20))
  (Exactly -> y) <- forAll (Gen.int (Range.linear 0 20))

  let ( _, _, z ) = eqR ( x, y, mempty )
  z === Exactly (x == y)

hprop_eqR_eqR :: Property
hprop_eqR_eqR = EqR.eqR_eqR defined_int

hprop_eqR_lifted :: Property
hprop_eqR_lifted = property do
  x <- forAll defined_int
  y <- forAll defined_int

  let ( _, _, z ) = eqR ( x, y, mempty )
  z === liftA2 (==) x y

hprop_eqR_reflexivity :: Property
hprop_eqR_reflexivity = property do
  x <- forAll defined_int

  let ( _, _, c ) = eqR ( x, x, mempty )
  assert (x == Conflict || c <> trueR == trueR)

hprop_eqR_symmetry :: Property
hprop_eqR_symmetry = EqR.eqR_symmetry defined_int

hprop_eqR_neR :: Property
hprop_eqR_neR = EqR.eqR_neR defined_int

hprop_eqR_neR_simple :: Property
hprop_eqR_neR_simple = property do
  (Exactly -> x) <- forAll (Gen.int (Range.linear 0 20))
  (Exactly -> y) <- forAll (Gen.int (Range.linear 0 20))

  let ( _, _, z ) = neR ( x, y, mempty )
  z === Exactly (x /= y)

hprop_ordR_gteR_simple :: Property
hprop_ordR_gteR_simple = property do
  (Exactly -> x) <- forAll (Gen.int (Range.linear 0 20))
  (Exactly -> y) <- forAll (Gen.int (Range.linear 0 20))

  let ( _, _, z ) = gteR ( x, y, mempty )
  z === Exactly (x >= y)

hprop_ordR_gtR_simple :: Property
hprop_ordR_gtR_simple = property do
  (Exactly -> x) <- forAll (Gen.int (Range.linear 0 20))
  (Exactly -> y) <- forAll (Gen.int (Range.linear 0 20))

  let ( _, _, z ) = gtR ( x, y, mempty )
  z === Exactly (x > y)

hprop_ordR_lteR :: Property
hprop_ordR_lteR = OrdR.ordR_lteR defined_int

hprop_ordR_lteR_simple :: Property
hprop_ordR_lteR_simple = property do
  (Exactly -> x) <- forAll (Gen.int (Range.linear 0 20))
  (Exactly -> y) <- forAll (Gen.int (Range.linear 0 20))

  let ( _, _, z ) = lteR ( x, y, mempty )
  z === Exactly (x <= y)

hprop_ordR_ltR_simple :: Property
hprop_ordR_ltR_simple = property do
  (Exactly -> x) <- forAll (Gen.int (Range.linear 0 20))
  (Exactly -> y) <- forAll (Gen.int (Range.linear 0 20))

  let ( _, _, z ) = ltR ( x, y, mempty )
  z === Exactly (x < y)

hprop_ordR_symmetry :: Property
hprop_ordR_symmetry = OrdR.ordR_symmetry defined_int

hprop_ordR_reflexivity :: Property
hprop_ordR_reflexivity = property do
  x <- forAll defined_int

  let ( _, _, c ) = lteR ( x, x, mempty )
  assert (x == Conflict || c <> trueR == trueR)

hprop_sumR_addR :: Property
hprop_sumR_addR = SumR.sumR_addR defined_int

hprop_sumR_addR_simple :: Property
hprop_sumR_addR_simple = property do
  (Exactly -> x) <- forAll (Gen.int (Range.linear 0 10))
  (Exactly -> y) <- forAll (Gen.int (Range.linear 0 10))

  let ( _, _, z ) = addR ( x, y, mempty )
  z === x + y

hprop_sumR_negateR_simple :: Property
hprop_sumR_negateR_simple = property do
  (Exactly -> x) <- forAll (Gen.int (Range.linear 0 10))

  let ( _, y ) = negateR ( x, mempty )
  y === negate x

hprop_sumR_subR_simple :: Property
hprop_sumR_subR_simple = property do
  (Exactly -> x) <- forAll (Gen.int (Range.linear 0 10))
  (Exactly -> y) <- forAll (Gen.int (Range.linear 0 10))

  let ( _, _, z ) = subR ( x, y, mempty )
  z === x - y

spec_dinesman :: Spec
spec_dinesman = it "dinesman" do
  let baker, cooper, fletcher, miller, smith :: Defined Int
      baker    = 3
      cooper   = 2
      fletcher = 4
      miller   = 5
      smith    = 1

  let ( _, _, a ) = neR  ( baker, 5, mempty ) -- *
      ( _, _, b ) = neR  ( cooper, 1, mempty ) -- *
      ( _, _, c ) = neR  ( fletcher, 1, mempty )
      ( _, _, d ) = neR  ( fletcher, 5, mempty )
      ( _, _, e ) = andR ( c, d, mempty ) -- *
      ( _, _, f ) = gtR  ( miller, cooper, mempty ) -- *
      ( _, _, g ) = subR ( smith, fletcher, mempty )
      ( _,    h ) = absR ( g, mempty )
      ( _, _, i ) = neR  ( h, 1, mempty) -- *
      ( _, _, j ) = subR ( fletcher, cooper, mempty )
      ( _,    k ) = absR ( j, mempty )
      ( _, _, l ) = neR  ( k, 1, mempty) -- *

  ( a, b, e, f, i, l ) `shouldBe`
    ( trueR, trueR, trueR, trueR, trueR, trueR )
