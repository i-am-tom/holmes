{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Data.JoinSemilattice.Class.Ord
Description : Relationships between values and their comparison results.
Copyright   : (c) Tom Harding, 2020
License     : MIT
-}
module Data.JoinSemilattice.Class.Ord where

import Control.Applicative (liftA2)
import Data.JoinSemilattice.Defined (Defined (..))
import Data.JoinSemilattice.Intersect (Intersect (..), Intersectable)
import qualified Data.JoinSemilattice.Intersect as Intersect
import Data.JoinSemilattice.Class.Boolean (BooleanR (..))
import Data.JoinSemilattice.Class.Eq (EqR (..), EqC')
import Data.Kind (Constraint, Type)

-- | Comparison relationships between two values and their comparison result.
-- See 'Data.JoinSemilattice.Class.Eq.EqR' for more information on the design
-- of this class, and a full apology for the constraints involved.
class (EqR f, forall x. OrdC f x => EqC' f x) => OrdR (f :: Type -> Type) where
  type OrdC f :: Type -> Constraint
  type OrdC f = EqC f

  -- | A relationship between two values and whether the left is less than or
  -- equal to the right.
  lteR :: OrdC f x => ( f x, f x, f Bool ) -> ( f x, f x, f Bool )

-- | Comparison between two values and their '(>)' result.
gtR :: (OrdR f, OrdC f x) => ( f x, f x, f Bool ) -> ( f x, f x, f Bool )
gtR ( x, y, z ) = let ( y', x', z' ) = ltR ( y, x, z ) in ( x', y', z' )

-- | Comparison between two values and their '(>=)' result.
gteR :: (OrdR f, OrdC f x) => ( f x, f x, f Bool ) -> ( f x, f x, f Bool )
gteR ( x, y, z ) = let ( y', x', z' ) = lteR ( y, x, z ) in ( x', y', z' )

-- | Comparison between two values and their '(<)' result.
ltR :: (OrdR f, OrdC f x) => ( f x, f x, f Bool ) -> ( f x, f x, f Bool )
ltR ( x, y, z )
  = let ( notZ', _ ) = notR ( mempty, z )
        ( x', y', notZR ) = gteR ( x, y, notZ' )
        ( _, z' ) = notR ( notZR, mempty )

    in ( x', y', z' )

instance OrdR Defined where
  type OrdC Defined = Ord

  lteR ( x, y, _ ) = ( mempty, mempty, liftA2 (<=) x y )

class (Ord x, Intersectable x) => OrdIntersectable (x :: Type)
instance (Ord x, Intersectable x) => OrdIntersectable x

instance OrdR Intersect where
  type OrdC Intersect = OrdIntersectable

  lteR ( x, y, z )
    = ( if | z == trueR  -> Intersect.filter (<= maximum y) x
           | z == falseR -> Intersect.filter ( > minimum y) x
           | otherwise   -> mempty

      , if | z == trueR  -> Intersect.filter (>= minimum x) y
           | z == falseR -> Intersect.filter ( < maximum x) y
           | otherwise   -> mempty

      , Intersect.lift2 (<=) x y
      )
