{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}

{-|
Module      : Data.JoinSemilattice.Class.Sum
Description : Relationships between values and their sums.
Copyright   : (c) Tom Harding, 2020
License     : MIT
-}
module Data.JoinSemilattice.Class.Sum where

import Data.JoinSemilattice.Class.Merge (Merge)
import Data.JoinSemilattice.Defined (Defined (..))
import Data.JoinSemilattice.Intersect (Intersect, Intersectable)
import Data.Kind (Type)

-- | A relationship between two values and their sum.
class Merge x => SumR (x :: Type) where
  addR :: ( x, x, x ) -> ( x, x, x )

  default addR :: Num x => ( x, x, x ) -> ( x, x, x )
  addR ( x, y, z ) = ( z - y, z - x, x + y )

-- | A relationship between two values and their difference.
subR :: SumR x => ( x, x, x ) -> ( x, x, x )
subR ( x, y, z ) = let ( z', y', x' ) = addR ( z, y, x ) in ( x', y', z' )

-- | A relationship between a value and its negation.
negateR :: (Num x, SumR x) => ( x, x ) -> ( x, x )
negateR ( x, y ) = let ( x', y', _ ) = addR ( x, y, 0 ) in ( x', y' )

instance (Eq x, Num x) => SumR (Defined x)
instance (Intersectable x, Num x) => SumR (Intersect x)
