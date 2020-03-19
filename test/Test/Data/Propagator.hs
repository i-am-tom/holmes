{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
module Test.Data.Propagator where

import qualified Control.Monad.Cell.Class as Cell
import Data.Holmes
import qualified Data.Propagator as Prop
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Prelude hiding (read)
import Test.Control.Monad.Cell.Class (Lestrade, read, scotlandYardSays)

hprop_eqR_reflexivity :: Property
hprop_eqR_reflexivity = property do
  x <- forAll (Gen.int (Range.linear 0 10))

  let x' :: MonadCell m => Prop m (Defined Int)
      x' = Prop.lift x

      program :: Lestrade h ()
      program = Prop.down (x' .== x')
            >>= \o -> Cell.write o (Exactly True)

  if scotlandYardSays program == Nothing
    then failure
    else success

hprop_eqR_negation :: Property
hprop_eqR_negation = property do
  x <- forAll (Gen.int (Range.linear 0 10))
  y <- forAll (Gen.int (Range.linear 0 10))

  let x', y' :: MonadCell m => Prop m (Defined Int)
      x' = Prop.lift x
      y' = Prop.lift y

      this :: Lestrade h ()
      this = Prop.down (x' .== y') >>= \o -> Cell.write o (Exactly True)

      that :: Lestrade h ()
      that = Prop.down (x' ./= y') >>= \o -> Cell.write o (Exactly False)

  scotlandYardSays this === scotlandYardSays that

hprop_eqR_simple :: Property
hprop_eqR_simple = property do
  x <- forAll (Gen.int (Range.linear 0 10))
  y <- forAll (Gen.int (Range.linear 0 10))

  let x', y' :: MonadCell m => Prop m (Defined Int)
      x' = Prop.lift x
      y' = Prop.lift y

      program :: Lestrade h (Defined Bool)
      program = Prop.down (x' .== y') >>= read

  scotlandYardSays program === Just (Exactly (x == y))

hprop_eqR_symmetry :: Property
hprop_eqR_symmetry = property do
  x <- forAll (Gen.int (Range.linear 0 10))
  y <- forAll (Gen.int (Range.linear 0 10))

  let x', y' :: MonadCell m => Prop m (Defined Int)
      x' = Prop.lift x
      y' = Prop.lift y

      this :: Lestrade h (Defined Bool)
      this = Prop.down (x' .== y') >>= read

      that :: Lestrade h (Defined Bool)
      that = Prop.down (y' .== x') >>= read

  scotlandYardSays this === scotlandYardSays that

hprop_ordR_negation :: Property
hprop_ordR_negation = property do
  x <- forAll (Gen.int (Range.linear 0 10))
  y <- forAll (Gen.int (Range.linear 0 10))

  let x', y' :: MonadCell m => Prop m (Defined Int)
      x' = Prop.lift x
      y' = Prop.lift y

      this :: Lestrade h (Defined Bool)
      this = Prop.down (x' .<= y') >>= read

      that :: Lestrade h (Defined Bool)
      that = Prop.down (x' .> y') >>= read

  scotlandYardSays this === fmap (fmap not) (scotlandYardSays that)

hprop_ordR_lteR_symmetry :: Property
hprop_ordR_lteR_symmetry = property do
  x <- forAll (Gen.int (Range.linear 0 10))
  y <- forAll (Gen.int (Range.linear 0 10))

  let x', y' :: MonadCell m => Prop m (Defined Int)
      x' = Prop.lift x
      y' = Prop.lift y

      this :: Lestrade h (Defined Bool)
      this = Prop.down (x' .<= y') >>= read

      that :: Lestrade h (Defined Bool)
      that = Prop.down (y' .>= x') >>= read

  scotlandYardSays this === scotlandYardSays that

hprop_ordR_ltR_symmetry :: Property
hprop_ordR_ltR_symmetry = property do
  x <- forAll (Gen.int (Range.linear 0 10))
  y <- forAll (Gen.int (Range.linear 0 10))

  let x', y' :: MonadCell m => Prop m (Defined Int)
      x' = Prop.lift x
      y' = Prop.lift y

      this :: Lestrade h (Defined Bool)
      this = Prop.down (x' .< y') >>= read

      that :: Lestrade h (Defined Bool)
      that = Prop.down (y' .> x') >>= read

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
