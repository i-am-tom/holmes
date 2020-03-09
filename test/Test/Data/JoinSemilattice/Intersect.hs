{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Test.Data.JoinSemilattice.Intersect where

import Data.Hashable (Hashable)
import Data.Holmes
import qualified Data.JoinSemilattice.Intersect as Intersect
import GHC.Generics (Generic)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Test.Data.Input.Config as Input
import qualified Test.Data.JoinSemilattice.Class.Boolean as BooleanR
import qualified Test.Data.JoinSemilattice.Class.Eq as EqR
import qualified Test.Data.JoinSemilattice.Class.Ord as OrdR
import qualified Test.Util.Laws as Laws

data Weekday
  = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (Hashable)

intersect_weekday :: Gen (Intersect Weekday)
intersect_weekday = do
  list <- Gen.list (Range.linear 0 4) Gen.enumBounded
  pure (Intersect.fromList list)

intersect_weekday_unconflicted :: Gen (Intersect Weekday)
intersect_weekday_unconflicted = do
  list <- Gen.list (Range.linear 1 4) Gen.enumBounded
  pure (Intersect.fromList list)

hprop_from_fill :: Property
hprop_from_fill = Input.from_fill @(Intersect Int)

hprop_semigroup_associativity :: Property
hprop_semigroup_associativity = Laws.semigroup_associativity intersect_weekday

hprop_monoid_identity :: Property
hprop_monoid_identity = Laws.monoid_identity intersect_weekday

hprop_join_semilattice_commutativity :: Property
hprop_join_semilattice_commutativity = Laws.semigroup_commutativity intersect_weekday

hprop_join_semilattice_idempotence :: Property
hprop_join_semilattice_idempotence = Laws.semigroup_idempotence intersect_weekday

hprop_booleanR_andR_simple :: Property
hprop_booleanR_andR_simple = property do
  x <- forAll Gen.bool
  y <- forAll Gen.bool

  let ( _, _, z ) = andR ( Exactly x, Exactly y, mempty )
  z === Exactly (x && y)

hprop_booleanR_andR :: Property
hprop_booleanR_andR = BooleanR.booleanR_andR @(Defined Bool)

hprop_booleanR_deMorgan_and :: Property
hprop_booleanR_deMorgan_and = BooleanR.booleanR_deMorgan_and @(Defined Bool)

hprop_booleanR_deMorgan_or :: Property
hprop_booleanR_deMorgan_or = BooleanR.booleanR_deMorgan_or @(Defined Bool)

hprop_booleanR_notR :: Property
hprop_booleanR_notR = BooleanR.booleanR_notR @(Defined Bool)

hprop_booleanR_orR :: Property
hprop_booleanR_orR = BooleanR.booleanR_orR @(Defined Bool)

hprop_eqR_simple :: Property
hprop_eqR_simple = property do
  (Exactly -> x) <- forAll (Gen.int (Range.linear 0 20))
  (Exactly -> y) <- forAll (Gen.int (Range.linear 0 20))

  let ( _, _, z ) = eqR ( x, y, mempty )
  z === Exactly (x == y)

hprop_eqR_eqR :: Property
hprop_eqR_eqR = EqR.eqR_eqR intersect_weekday

hprop_eqR_reflexivity :: Property
hprop_eqR_reflexivity = EqR.eqR_reflexivity intersect_weekday_unconflicted

hprop_eqR_symmetry :: Property
hprop_eqR_symmetry = EqR.eqR_symmetry intersect_weekday

hprop_eqR_negation :: Property
hprop_eqR_negation = EqR.eqR_negation intersect_weekday

hprop_ordR_lteR :: Property
hprop_ordR_lteR = OrdR.ordR_lteR intersect_weekday

hprop_ordR_lteR_simple :: Property
hprop_ordR_lteR_simple = property do
  (Exactly -> x) <- forAll (Gen.int (Range.linear 0 20))
  (Exactly -> y) <- forAll (Gen.int (Range.linear 0 20))

  let ( _, _, z ) = lteR ( x, y, mempty )
  z === Exactly (x <= y)
