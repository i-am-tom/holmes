{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiWayIf #-}

{-|
Module      : Data.JoinSemilattice.Class.Boolean
Description : Relationships between boolean variables.
Copyright   : (c) Tom Harding, 2020
License     : MIT
-}
module Data.JoinSemilattice.Class.Boolean where

import Control.Applicative (liftA2)
import Data.JoinSemilattice.Class.Merge (Merge)
import Data.JoinSemilattice.Defined (Defined (..))
import Data.JoinSemilattice.Intersect (Intersect (..))
import qualified Data.JoinSemilattice.Intersect as Intersect
import Data.Kind (Type)

-- | Rather than the 'not', 'and', and 'or' functions we know and love, the
-- 'BooleanR' class presents /relationships/ that are analogous to these. The
-- main difference is that relationships are not one-way. For example, if I
-- tell you that the /output/ of @x && y@ is 'True', you can tell me what the
-- inputs are, even if your computer can't. The implementations of 'BooleanR'
-- should be such that all directions of inference are considered.
class Merge x => BooleanR (x :: Type) where
  -- | An overloaded 'False' value.
  falseR :: x

  -- | An overloaded 'True' value.
  trueR :: x

  -- | A relationship between a boolean value and its opposite.
  notR :: ( x, x ) -> ( x, x )

  -- | A relationship between two boolean values and their conjunction.
  andR :: ( x, x, x ) -> ( x, x, x )

  -- | A relationship between two boolean values and their disjunction.
  orR :: ( x, x, x ) -> ( x, x, x )

instance BooleanR (Defined Bool) where
  falseR = Exactly False
  trueR  = Exactly True

  notR (x, y) = ( fmap not y, fmap not x )

  andR (x, y, z)
    = ( if | z == trueR                -> trueR
           | z == falseR && y == trueR -> falseR
           | otherwise                 -> mempty

      , if | z == trueR                -> trueR
           | z == falseR && x == trueR -> falseR
           | otherwise                 -> mempty

      , liftA2 (&&) x y
      )

  orR (x, y, z)
    = ( if | z == falseR               -> falseR
           | z == trueR && y == falseR -> trueR
           | otherwise                 -> mempty

      , if | z == falseR               -> falseR
           | z == trueR && x == falseR -> trueR
           | otherwise                 -> mempty

      , liftA2 (||) x y
      )

instance BooleanR (Intersect Bool) where
  falseR = Intersect.singleton False
  trueR  = Intersect.singleton True

  notR (x, y) = ( Intersect.map not y, Intersect.map not x )

  andR (x, y, z)
    = ( if | z == trueR                -> trueR
           | z == falseR && y == trueR -> falseR
           | otherwise                 -> mempty

      , if | z == trueR                -> trueR
           | z == falseR && x == trueR -> falseR
           | otherwise                 -> mempty

      , Intersect.lift2 (&&) x y
      )

  orR (x, y, z)
    = ( if | z == falseR               -> falseR
           | z == trueR && y == falseR -> trueR
           | otherwise                 -> mempty

      , if | z == falseR               -> falseR
           | z == trueR && x == falseR -> trueR
           | otherwise                 -> mempty

      , Intersect.lift2 (||) x y
      )
