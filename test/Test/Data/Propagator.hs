{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
module Test.Data.Propagator where

import qualified Control.Monad.Cell.Class as Cell
import Data.Holmes
import qualified Data.Propagator as Prop
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.List (nub)
import Prelude hiding (read)
import Test.Control.Monad.Cell.Class (Lestrade, read, scotlandYardSays)

hprop_eqR_reflexivity :: Property
hprop_eqR_reflexivity = property do
  x <- forAll (Gen.int (Range.linear 0 10))

  let program :: Lestrade h ()
      program = Prop.down (Prop.lift x .== Prop.lift x)
            >>= \o -> Cell.write o (Exactly True)

  if scotlandYardSays program == Nothing
    then failure
    else success

hprop_eqR_negation :: Property
hprop_eqR_negation = property do
  x <- forAll (Gen.int (Range.linear 0 10))
  y <- forAll (Gen.int (Range.linear 0 10))

  let this :: Lestrade h ()
      this = Prop.down (Prop.lift x .== Prop.lift y)
         >>= \o -> Cell.write o (Exactly True)

      that :: Lestrade h ()
      that = Prop.down (Prop.lift x ./= Prop.lift y)
         >>= \o -> Cell.write o (Exactly False)

  scotlandYardSays this === scotlandYardSays that

hprop_eqR_simple :: Property
hprop_eqR_simple = property do
  x <- forAll (Gen.int (Range.linear 0 10))
  y <- forAll (Gen.int (Range.linear 0 10))

  let program :: Lestrade h (Defined Bool)
      program = Prop.down (Prop.lift x .== Prop.lift y) >>= read

  scotlandYardSays program === Just (Exactly (x == y))

hprop_eqR_symmetry :: Property
hprop_eqR_symmetry = property do
  x <- forAll (Gen.int (Range.linear 0 10))
  y <- forAll (Gen.int (Range.linear 0 10))

  let this :: Lestrade h (Defined Bool)
      this = Prop.down (Prop.lift x .== Prop.lift y) >>= read

      that :: Lestrade h (Defined Bool)
      that = Prop.down (Prop.lift y .== Prop.lift x) >>= read

  scotlandYardSays this === scotlandYardSays that

hprop_ordR_negation :: Property
hprop_ordR_negation = property do
  x <- forAll (Gen.int (Range.linear 0 10))
  y <- forAll (Gen.int (Range.linear 0 10))

  let this :: Lestrade h (Defined Bool)
      this = Prop.down (Prop.lift x .<= Prop.lift y) >>= read

      that :: Lestrade h (Defined Bool)
      that = Prop.down (Prop.lift x .> Prop.lift y) >>= read

  scotlandYardSays this === fmap (fmap not) (scotlandYardSays that)

hprop_ordR_lteR_symmetry :: Property
hprop_ordR_lteR_symmetry = property do
  x <- forAll (Gen.int (Range.linear 0 10))
  y <- forAll (Gen.int (Range.linear 0 10))

  let this :: Lestrade h (Defined Bool)
      this = Prop.down (Prop.lift x .<= Prop.lift y) >>= read

      that :: Lestrade h (Defined Bool)
      that = Prop.down (Prop.lift y .>= Prop.lift x) >>= read

  scotlandYardSays this === scotlandYardSays that

hprop_ordR_ltR_symmetry :: Property
hprop_ordR_ltR_symmetry = property do
  x <- forAll (Gen.int (Range.linear 0 10))
  y <- forAll (Gen.int (Range.linear 0 10))

  let this :: Lestrade h (Defined Bool)
      this = Prop.down (Prop.lift x .< Prop.lift y) >>= read

      that :: Lestrade h (Defined Bool)
      that = Prop.down (Prop.lift y .> Prop.lift x) >>= read

  scotlandYardSays this === scotlandYardSays that

hprop_ordR_simple :: Property
hprop_ordR_simple = property do
  x <- forAll (Gen.int (Range.linear 0 10))
  y <- forAll (Gen.int (Range.linear 0 10))

  let x', y' :: MonadCell m => Prop m (Defined Int)
      x' = Prop.lift x
      y' = Prop.lift y

      program :: Lestrade h (Defined Bool)
      program = Prop.down (x' .<= y') >>= read

  scotlandYardSays program === Just (Exactly (x <= y))

hprop_choice_unique_choices :: Property
hprop_choice_unique_choices = property do
  n <- forAll $ Gen.int (Range.linear 0 6)
  k <- forAll $ Gen.int (Range.linear 0 n)

  let choices = Prop.choose n k

  nub choices === choices

hprop_choice_correct_number_of_choices :: Property
hprop_choice_correct_number_of_choices = property do
  n <- forAll $ Gen.int (Range.linear 0 6)
  k <- forAll $ Gen.int (Range.linear 0 n)

  let choices = Prop.choose n k
      factorial 0 = 1
      factorial m = m * factorial (pred m)

  length choices === factorial n `div` (factorial k * factorial (n-k))

hprop_exactly_simple :: Property
hprop_exactly_simple = property do
  xs <- forAll (Gen.list (Range.linear 0 10) (Gen.int (Range.linear 0 10)))
  k  <- forAll (Gen.int (Range.linear 0 (length xs)))
  x  <- forAll (Gen.int (Range.linear 0 10))

  let x' :: MonadCell m => Prop m (Defined Int)
      x' = Prop.lift x
      
      xs' :: MonadCell m => [Prop m (Defined Int)]
      xs' = map Prop.lift xs

      program :: Lestrade h (Defined Bool)
      program = Prop.down (Prop.exactly k (.== x') xs') >>= read

      expected = length (filter (==x) xs) == k

  scotlandYardSays program === Just (Exactly expected)
