{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiWayIf #-}

{-|
Module      : Data.JoinSemilattice.Class.Ord
Description : Relationships between values and their comparison results.
Copyright   : (c) Tom Harding, 2020
License     : MIT
-}
module Data.JoinSemilattice.Class.Ord where

import Control.Applicative (liftA2)
import Data.Hashable (Hashable)
import Data.JoinSemilattice.Defined (Defined (..))
import Data.JoinSemilattice.Intersect (Intersect (..))
import qualified Data.JoinSemilattice.Intersect as Intersect
import Data.JoinSemilattice.Class.Boolean (BooleanR (..))
import Data.JoinSemilattice.Class.Eq (EqR)
import Data.Kind (Type)

-- | Comparison relationships between two values and their comparison result.
class EqR x b => OrdR (x :: Type) (b :: Type) | x -> b where

  -- | A relationship between two values and whether the left is less than or
  -- equal to the right.
  lteR :: ( x, x, b ) -> ( x, x, b )

-- | Comparison between two values and their '(>)' result.
gtR :: OrdR x b => ( x, x, b ) -> ( x, x, b )
gtR ( x, y, z ) = let ( y', x', z' ) = ltR ( y, x, z ) in ( x', y', z' )

-- | Comparison between two values and their '(>=)' result.
gteR :: OrdR x b => ( x, x, b ) -> ( x, x, b )
gteR ( x, y, z ) = let ( y', x', z' ) = lteR ( y, x, z ) in ( x', y', z' )

-- | Comparison between two values and their '(<)' result.
ltR :: OrdR x b => ( x, x, b ) -> ( x, x, b )
ltR ( x, y, z )
  = let ( notZ', _ ) = notR ( mempty, z )
        ( x', y', notZR ) = gteR ( x, y, notZ' )
        ( _, z' ) = notR ( notZR, mempty )

    in ( x', y', z' )

instance Ord x => OrdR (Defined x) (Defined Bool) where
  lteR ( x, y, _ ) = ( mempty, mempty, liftA2 (<=) x y )

instance (Bounded x, Enum x, Hashable x, Ord x)
    => OrdR (Intersect x) (Intersect Bool) where
  lteR ( x, y, z )
    = ( if | z == trueR  -> Intersect.filter (<= maximum y) x
           | z == falseR -> Intersect.filter ( > minimum y) x
           | otherwise   -> mempty

      , if | z == trueR  -> Intersect.filter (>= minimum x) y
           | z == falseR -> Intersect.filter ( < maximum x) y
           | otherwise   -> mempty

      , Intersect.lift2 (<=) x y
      )
