{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
module Test.Data.JoinSemilattice.Intersect where

import Data.Hashable (Hashable)
import Data.JoinSemilattice.Intersect (Intersect)
import qualified Data.JoinSemilattice.Intersect as Intersect
import GHC.Generics (Generic)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Test.Data.JoinSemilattice.Class.Boolean as BooleanR
import qualified Test.Data.JoinSemilattice.Class.Eq as EqR
import qualified Test.Util.Laws as Laws

data Weekday
  = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (Hashable)

intersect_weekday :: Gen (Intersect Weekday)
intersect_weekday = do
  list <- Gen.list (Range.linear 0 4) Gen.enumBounded
  pure (Intersect.fromList list)

hprop_semigroup_associativity :: Property
hprop_semigroup_associativity = Laws.semigroup_associativity intersect_weekday

hprop_monoid_identity :: Property
hprop_monoid_identity = Laws.monoid_identity intersect_weekday

hprop_join_semilattice_commutativity :: Property
hprop_join_semilattice_commutativity = Laws.semigroup_commutativity intersect_weekday

hprop_join_semilattice_idempotence :: Property
hprop_join_semilattice_idempotence = Laws.semigroup_idempotence intersect_weekday

hprop_booleanR_andR :: Property
hprop_booleanR_andR = BooleanR.booleanR_andR @(Intersect Bool)

hprop_booleanR_notR :: Property
hprop_booleanR_notR = BooleanR.booleanR_notR @(Intersect Bool)

hprop_booleanR_orR :: Property
hprop_booleanR_orR = BooleanR.booleanR_orR @(Intersect Bool)

hprop_eqR_eqR :: Property
hprop_eqR_eqR = EqR.eqR_eqR intersect_weekday

hprop_eqR_neR :: Property
hprop_eqR_neR = EqR.eqR_neR intersect_weekday
