{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE KindSignatures #-}

{-|
Module      : Data.JoinSemilattice.Class.Fractional
Description : Relationships between values and their (floating/fractional) product.
Copyright   : (c) Tom Harding, 2020
License     : MIT
-}
module Data.JoinSemilattice.Class.Fractional where

import Data.Hashable (Hashable)
import Data.JoinSemilattice.Defined (Defined)
import Data.JoinSemilattice.Intersect (Intersect)
import Data.JoinSemilattice.Class.Sum (SumR)
import Data.Kind (Type)

-- | Reversible (fractional or floating-point) multiplication as a three-value
-- relationship between two values and their product.
class SumR x => FractionalR (x :: Type) where
  multiplyR :: ( x, x, x ) -> ( x, x, x )

  default multiplyR :: Fractional x => ( x, x, x ) -> ( x, x, x )
  multiplyR ( x, y, z ) = ( z / y, z / x, x * y )

-- | A three-way division relationships implemented as a flipped multiplication
-- relationship.
divideR :: FractionalR x => ( x, x, x ) -> ( x, x, x )
divideR ( x, y, z ) = let ( x', y', z' ) = multiplyR ( z, y, x ) in ( x', y', z' )

-- | A two-way relationship between a value and its reciprocal, implemented
-- with a multiplication relationship in which the third value is fixed to be
-- @1@.
recipR :: (FractionalR x, Num x) => ( x, x ) -> ( x, x )
recipR ( x, y ) = let ( x', y', _ ) = multiplyR ( x, y, 1 ) in ( x', y' )

instance (Eq x, Fractional x) => FractionalR (Defined x)

instance (Bounded x, Enum x, Eq x, Fractional x, Hashable x)
  => FractionalR (Intersect x)
