{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Util.Laws where

import Control.Applicative (liftA2)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

semigroup_associativity :: (Eq x, Semigroup x, Show x) => Gen x -> Property
semigroup_associativity gen = property do
  x <- forAll gen
  y <- forAll gen
  z <- forAll gen

  x <> (y <> z) === (x <> y) <> z

monoid_identity :: (Eq x, Monoid x, Show x) => Gen x -> Property
monoid_identity gen = property do
  x <- forAll gen
  mempty <> x === x

semigroup_commutativity :: (Eq x, Semigroup x, Show x) => Gen x -> Property
semigroup_commutativity gen = property do
  x <- forAll gen
  y <- forAll gen

  x <> y === y <> x

semigroup_idempotence :: (Eq x, Semigroup x, Show x) => Gen x -> Property
semigroup_idempotence gen = property do
  x <- forAll gen

  x <> x === x

functor_identity :: (Eq (f x), Functor f, Show (f x)) => Gen (f x) -> Property
functor_identity gen = property do
  x <- forAll gen

  fmap id x === x

functor_composition :: (Eq (f Int), Functor f, Show (f Int)) => Gen (f Int) -> Property
functor_composition gen = property do
  x <- forAll gen
  y <- forAll $ Gen.int (Range.linear 0 100)
  z <- forAll $ Gen.int (Range.linear 0 100)

  fmap (+ y) (fmap (* z) x) === fmap ((+ y) . (* z)) x

applicative_identity :: (Applicative f, Eq (f x), Show (f x)) => Gen (f x) -> Property
applicative_identity gen = property do
  x <- forAll gen

  (pure id <*> x) === x

applicative_composition :: (Applicative f, Eq (f Int), Show (f Int)) => Gen (f Int) -> Property
applicative_composition gen = property do
  x <- forAll gen
  y <- forAll gen
  z <- forAll gen

  liftA2 (+) x (liftA2 (+) y z) === liftA2 (+) (liftA2 (+) x y) z

applicative_homomorphism :: forall f. (Applicative f, Eq (f Int), Show (f Int)) => Property
applicative_homomorphism = property do
  x <- forAll (Gen.int (Range.linear 0 10))
  y <- forAll (Gen.int (Range.linear 0 10))

  (pure (+) <*> pure x <*> pure y)
    === pure @f (x + y)

applicative_interchange :: (Applicative f, Eq (f Int), Show (f Int)) => Gen (f Int) -> Property
applicative_interchange gen = property do
  x <- forAll (Gen.int (Range.linear 0 10))
  y <- forAll gen

  let f = fmap (+) y
  (f <*> pure x) === (pure ($ x) <*> f)
