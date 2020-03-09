{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE KindSignatures #-}

{-|
Module      : Data.JoinSemilattice.Class.Integral
Description : Relationships between values and their (integral) division results.
Copyright   : (c) Tom Harding, 2020
License     : MIT
-}
module Data.JoinSemilattice.Class.Integral where

import Data.Hashable (Hashable)
import Data.JoinSemilattice.Defined (Defined (..))
import Data.JoinSemilattice.Intersect (Intersect)
import qualified Data.JoinSemilattice.Intersect as Intersect
import Data.JoinSemilattice.Class.Sum (SumR)
import Data.Kind (Type)

-- | A four-way 'divMod' relationship between two values, the result of
-- integral division, and the result of the first modulo the second.
class SumR x => IntegralR (x :: Type) where
  divModR :: ( x, x, x, x ) -> ( x, x, x, x )

-- | Integral multiplication implemented as a 'divModR' relationship in which
-- the remainder is fixed to be @0@.
timesR :: (IntegralR x, Num x) => ( x, x, x ) -> ( x, x, x )
timesR ( x, y, z ) = let ( z', y', x', _ ) = divModR ( z, y, x, 0 ) in ( x', y', z' )

-- | Integal division as a three-value relationship.
divR :: IntegralR x => ( x, x, x ) -> ( x, x, x )
divR ( x, y, z ) = let ( x', y', z', _ ) = divModR ( x, y, z, mempty ) in ( x', y', z' )

-- | Modulo operator implemented as a three-value relationship.
modR :: IntegralR x => ( x, x, x ) -> ( x, x, x )
modR ( x, y, z ) = let ( x', y', _, z' ) = divModR ( x, y, mempty, z ) in ( x', y', z' )

instance (Eq x, Integral x) => IntegralR (Defined x) where
  divModR ( x, y, z, w )
    = (  y * z + w
      , (x - w) `div` z
      , (x - w) `div` y
      ,  x - (y * z)
      )

instance (Bounded x, Enum x, Eq x, Hashable x, Integral x)
    => IntegralR (Intersect x) where
  divModR ( x, y, z, w )
    = ( y * z + w
      , Intersect.lift2 div (x - w) z
      , Intersect.lift2 div (x - w) y
      , x - (y * z)
      )
