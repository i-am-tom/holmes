{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Data.JoinSemilattice.Defined
Description : Values with differing levels of "definedness".
Copyright   : (c) Tom Harding, 2020
License     : MIT

The 'Defined' type simplifies the join semilattice-shaped knowledge down to its
simplest form, by saying there are only three possible states of knowledge:

- I don't know anything about this value.
- I know exactly what this value is.
- I'm getting conflicting information.

The simplicity of the type makes it incredibly helpful when we're trying to
lift regular computations into the world of propagators.
-}
module Data.JoinSemilattice.Defined where

import Control.Applicative (liftA2)
import Data.Hashable (Hashable)
import Data.Input.Config (Config (..), Input (..))
import Data.Kind (Type)
import Data.List.NonEmpty (unzip)
import Data.Monoid (Ap (..))
import GHC.Generics (Generic)
import Prelude hiding (unzip)

-- | Defines simple "levels of knowledge" about a value.
data Defined (x :: Type)
  = Unknown   -- ^ Nothing has told me what this value is.
  | Exactly x -- ^ Everyone who has told me this value agrees.
  | Conflict  -- ^ Two sources disagree on what this value should be.
  deriving stock (Eq, Ord, Show, Functor, Generic)
  deriving anyclass (Hashable)
  deriving (Bounded, Num) via (Ap Defined x)

class (Eq content, Ord content)
  => Definable content

instance (Eq content, Ord content)
  => Definable content

instance Enum content => Enum (Defined content) where
  fromEnum = \case
    Exactly this -> fromEnum this
    _            -> error "fromEnum is undefined for non-exact values."

  toEnum = pure . toEnum

instance Applicative Defined where
  pure = Exactly

  Conflict <*> _ = Conflict
  _ <*> Conflict = Conflict

  Unknown <*> _ = Unknown
  _ <*> Unknown = Unknown

  Exactly f <*> Exactly x
    = Exactly (f x)

instance Eq content => Semigroup (Defined content) where
  Conflict <> _ = Conflict
  _ <> Conflict = Conflict

  this <> Unknown = this
  Unknown <> that = that

  Exactly this <> Exactly that
    | this == that = Exactly this
    | otherwise    = Conflict

instance Eq content => Monoid (Defined content) where
  mempty = Unknown

instance Real content => Real (Defined content) where
  toRational = \case
    Exactly this -> toRational this
    _            -> error "toRational is undefined for non-exact values."

instance Integral content => Integral (Defined content) where
  quotRem this that = unzip (liftA2 quotRem this that)

  toInteger = \case
    Exactly this -> toInteger this
    _            -> error "toInteger is undefined for non-exact values."

instance Fractional x => Fractional (Defined x) where
  (/) = liftA2 (/)

  fromRational = pure . fromRational
  recip        = fmap recip

instance Input (Defined content) where
  type Raw (Defined content) = content

  from count options = Config (replicate count Unknown) do
    pure . \case
      Unknown -> map Exactly options
      decided -> [ decided ]
