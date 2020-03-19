{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}

{-|
Module      : Data.JoinSemilattice.Class.Lifting
Description : Lifting values into parameter types.
Copyright   : (c) Tom Harding, 2020
License     : MIT
-}
module Data.JoinSemilattice.Class.Lifting where

import Data.Hashable (Hashable)
import Data.JoinSemilattice.Defined (Defined (..))
import Data.JoinSemilattice.Intersect (Intersect)
import qualified Data.JoinSemilattice.Intersect as Intersect
import Data.Kind (Constraint, Type)

-- | Embed a regular value inside a parameter type.
class Lifting (f :: Type -> Type) (c :: Type -> Constraint) | f -> c where
  lift' :: c x => x -> f x

class Trivial (x :: Type)
instance Trivial x

instance Lifting Defined Trivial where
  lift' = Exactly

instance Lifting Intersect Hashable where
  lift' = Intersect.singleton
