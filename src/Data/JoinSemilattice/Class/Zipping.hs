{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}

{-|
Module      : Data.JoinSemilattice.Class.Zipping
Description : Computing knowledge from multiple parameters.
Copyright   : (c) Tom Harding, 2020
License     : MIT
-}
module Data.JoinSemilattice.Class.Zipping (Zipping (..)) where

import Control.Applicative (liftA2)
import Data.JoinSemilattice.Class.Mapping (Mapping)
import Data.JoinSemilattice.Defined (Defined)
import Data.JoinSemilattice.Intersect (Intersect, Intersectable)
import qualified Data.JoinSemilattice.Intersect as Intersect
import Data.Kind (Constraint, Type)

-- | Lift a relationship between three values over some @f@ (usually a
-- parameter type).
class Mapping f c => Zipping (f :: Type -> Type) (c :: Type -> Constraint) | f -> c where
  zipWithR
    :: (c x, c y, c z)
    => ( Maybe ((x, y) -> z)
       , Maybe ((x, z) -> y)
       , Maybe ((y, z) -> x)
       )
    -> ((f x, f y, f z) -> (f x, f y, f z))

instance Zipping Defined Eq where
  zipWithR (fs, gs, hs) (x, y, z)
    = ( case hs of Just h  -> liftA2 (curry h) y z
                   Nothing -> mempty

      , case gs of Just g  -> liftA2 (curry g) x z
                   Nothing -> mempty

      , case fs of Just f  -> liftA2 (curry f) x y
                   Nothing -> mempty
      )

instance Zipping Intersect Intersectable where
  zipWithR (fs, gs, hs) (x, y, z)
    = ( case hs of Just h  -> Intersect.lift2 (curry h) y z
                   Nothing -> mempty

      , case gs of Just g  -> Intersect.lift2 (curry g) x z
                   Nothing -> mempty

      , case fs of Just f  -> Intersect.lift2 (curry f) x y
                   Nothing -> mempty
      )
