{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Data.JoinSemilattice.Class.Mapping
Description : Lift "regular functions" over parameter types.
Copyright   : (c) Tom Harding, 2020
License     : MIT
-}
module Data.JoinSemilattice.Class.Mapping where

import Data.JoinSemilattice.Class.Merge (Merge)
import Data.JoinSemilattice.Defined (Defined (..))
import Data.JoinSemilattice.Intersect (Intersect, Intersectable)
import qualified Data.JoinSemilattice.Intersect as Intersect
import Data.Kind (Constraint, Type)

-- | Lift a relationship between two values over some type constructor.
-- Typically, this type constructor will be the parameter type.
class (forall x. c x => Merge (f x))
    => Mapping (f :: Type -> Type) (c :: Type -> Constraint) | f -> c where
  mapR :: (c x, c y) => (Maybe (x -> y), Maybe (y -> x)) -> ((f x, f y) -> (f x, f y))

instance Mapping Defined Eq where
  mapR ( fs, gs ) ( xs, ys )
    = ( case ys of
          Unknown   -> Unknown
          Conflict  -> Conflict
          Exactly y -> case gs of Just g  -> Exactly (g y)
                                  Nothing -> Unknown

      , case xs of
          Unknown   -> Unknown
          Conflict  -> Conflict
          Exactly x -> case fs of Just f  -> Exactly (f x)
                                  Nothing -> Unknown
      )

instance Mapping Intersect Intersectable where
  mapR ( fs, gs ) ( xs, ys )
    = ( case gs of Just g  -> Intersect.map g ys
                   Nothing -> mempty

      , case fs of Just f  -> Intersect.map f xs
                   Nothing -> mempty
      )
