{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-|
Module      : Data.JoinSemilattice.Class.Eq
Description : Equality relationships.
Copyright   : (c) Tom Harding, 2020
License     : MIT
-}
module Data.JoinSemilattice.Class.Eq where

import Control.Applicative (liftA2)
import Data.JoinSemilattice.Class.Boolean (BooleanR (..))
import Data.JoinSemilattice.Class.Merge (Merge)
import Data.JoinSemilattice.Defined (Defined (..))
import Data.JoinSemilattice.Intersect (Intersect (..), Intersectable)
import qualified Data.JoinSemilattice.Intersect as Intersect
import Data.Kind (Constraint, Type)

class EqC f x => EqC' f x
instance EqC f x => EqC' f x

-- | Equality between two variables as a relationship between them and their
-- result. The hope here is that, if we learn the output before the inputs, we
-- can often "work backwards" to learn something about them. If we know the
-- result is exactly /true/, for example, we can effectively then
-- 'Control.Monad.Cell.Class.unify' the two input cells, as we know that their
-- values will always be the same.
--
-- The class constraints are a bit ugly here, and it's something I'm hoping I
-- can tidy up down the line. The idea is that, previously, our class was
-- defined as:
--
-- @
--   class EqR (x :: Type) (b :: Type) | x -> b where
--     eqR :: (x -> x -> b) -> (x -> x -> b)
-- @
--
-- The problem here was that, if we said @x .== x :: Prop m (Defined Bool)@, we
-- couldn't even infer that the type of @x@ was @Defined@-wrapped, which made
-- the overloaded literals, for example, largely pointless.
--
-- To fix it, the class was rewritten to parameterise the wrapper type, which
-- means we can always make this inference. However, the constraints got a bit
-- grizzly when I hacked it together.
class (forall x. EqC' f x => Merge (f x), BooleanR f)
    => EqR (f :: Type -> Type) where
  type EqC f :: Type -> Constraint

  eqR :: EqC' f x => ( f x, f x, f Bool ) -> ( f x, f x, f Bool )

-- | A relationship between two variables and the result of a not-equals
-- comparison between them.
neR :: (EqR f, EqC' f x) => ( f x, f x, f Bool ) -> ( f x, f x, f Bool )
neR ( x, y, z )
  = let ( notZ', _ ) = notR ( mempty, z )
        ( x', y', notZR ) = eqR ( x, y, notZ' )
        ( _, z' ) = notR ( notZR, mempty )

    in ( x', y', z' )

instance EqR Defined where
  type EqC Defined = Eq

  eqR ( x, y, z )
    = ( if z == trueR then y else mempty
      , if z == trueR then x else mempty
      , liftA2 (==) x y
      )

instance EqR Intersect where
  type EqC Intersect = Intersectable

  eqR ( x, y, z )
    = ( if | z == trueR                           -> y
           | z == falseR && Intersect.size y == 1 -> Intersect.except y
           | otherwise                            -> mempty

      , if | z == trueR                           -> x
           | z == falseR && Intersect.size x == 1 -> Intersect.except x
           | otherwise                            -> mempty

      , Intersect.lift2 (==) x y
      )
