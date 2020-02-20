{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE KindSignatures #-}

{-|
Module      : Data.JoinSemilattice.Class.Abs
Description : Relationships between values and their absolutes.
Copyright   : (c) Tom Harding, 2020
License     : MIT
-}
module Data.JoinSemilattice.Class.Abs where

import Data.Hashable (Hashable)
import Data.JoinSemilattice.Class.Merge (Merge)
import Data.JoinSemilattice.Defined (Defined)
import Data.JoinSemilattice.Intersect (Intersect)
import qualified Data.JoinSemilattice.Intersect as Intersect
import Data.Kind (Type)

-- | Unlike the 'abs' we know, which is a /function/ from a value to its
-- absolute value, 'absR' is a /relationship/ between a value and its absolute.
class Merge x => AbsR (x :: Type) where

  -- | Given a value and its absolute, try to learn something in either
  -- direction.
  absR :: ( x, x ) -> ( x, x )

  -- | By default, this relationship is one-way.
  default absR :: Num x => ( x, x ) -> ( x, x )
  absR ( x, _ ) = ( mempty, abs x )

instance (Eq x, Num x) => AbsR (Defined x)

instance (Bounded x, Enum x, Eq x, Hashable x, Num x)
    => AbsR (Intersect x) where
  absR ( x, y ) = ( Intersect.union y (negate y), abs x )
