{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module      : Data.JoinSemilattice.Class.Mapping
Description : Lift "regular functions" over parameter types.
Copyright   : (c) Tom Harding, 2020
License     : MIT
-}
module Data.JoinSemilattice.Class.Mapping where

import Control.Applicative (liftA2)
import Data.JoinSemilattice.Class.Merge (Merge)
import Data.JoinSemilattice.Defined (Defined)
import Data.JoinSemilattice.Intersect (Intersect, Intersectable)
import qualified Data.JoinSemilattice.Intersect as Intersect
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (unzip)
import Prelude hiding (unzip)

-- | Lift a relationship between two values over some type constructor.
-- Typically, this type constructor will be the parameter type.
class (forall x. c x => Merge (f x))
    => Mapping (f :: Type -> Type) (c :: Type -> Constraint) | f -> c where
  mapR :: (c x, c y) => ((x, y) -> (x, y)) -> ((f x, f y) -> (f x, f y))

  default mapR :: Applicative f => ((x, y) -> (x, y)) -> ((f x, f y) -> (f x, f y))
  mapR f (xs, ys) = unzip (liftA2 (curry f) xs ys)

instance Mapping Defined Eq

instance Mapping Intersect Intersectable where
  mapR f (Intersect.toList -> xs, Intersect.toList -> ys) = do
    let ( xs', ys' ) = unzip (liftA2 (curry f) xs ys)

    ( Intersect.fromList xs', Intersect.fromList ys' )
