{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiWayIf #-}

{-|
Module      : Data.JoinSemilattice.Class.Eq
Description : Equality relationships.
Copyright   : (c) Tom Harding, 2020
License     : MIT
-}
module Data.JoinSemilattice.Class.Eq where

import Control.Applicative (liftA2)
import Data.Hashable (Hashable)
import Data.JoinSemilattice.Class.Boolean (BooleanR (..))
import Data.JoinSemilattice.Class.Merge (Merge)
import Data.JoinSemilattice.Defined (Defined (..))
import Data.JoinSemilattice.Intersect (Intersect (..))
import qualified Data.JoinSemilattice.Intersect as Intersect
import Data.Kind (Type)

-- | Equality between two variables as a relationship between them and their
-- result. The hope here is that, if we learn the output before the inputs, we
-- can often "work backwards" to learn something about them. If we know the
-- result is exactly /true/, for example, we can effectively then
-- 'Control.Monad.Cell.Class.unify' the two input cells, as we know that their
-- values will always be the same.
class (BooleanR b, Merge x) => EqR (x :: Type) (b :: Type) | x -> b where
  eqR :: ( x, x, b ) -> ( x, x, b )

-- | A relationship between two variables and the result of a not-equals
-- comparison between them.
neR :: EqR x b => ( x, x, b ) -> ( x, x, b )
neR ( x, y, z )
  = let ( notZ', _ ) = notR ( mempty, z )
        ( x', y', notZR ) = eqR ( x, y, notZ' )
        ( _, z' ) = notR ( notZR, mempty )

    in ( x', y', z' )

instance Eq x => EqR (Defined x) (Defined Bool) where
  eqR ( x, y, z )
    = ( if z == trueR then y else mempty
      , if z == trueR then x else mempty
      , liftA2 (==) x y
      )

instance (Bounded x, Enum x, Eq x, Hashable x)
    => EqR (Intersect x) (Intersect Bool) where
  eqR ( x, y, z )
    = ( if | z == trueR                           -> y
           | z == falseR && Intersect.size y == 1 -> Intersect.except y
           | otherwise                            -> mempty

      , if | z == trueR                           -> x
           | z == falseR && Intersect.size x == 1 -> Intersect.except x
           | otherwise                            -> mempty

      , Intersect.lift2 (==) x y
      )
