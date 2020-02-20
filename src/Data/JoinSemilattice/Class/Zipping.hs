{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module      : Data.JoinSemilattice.Class.Zipping
Description : Computing knowledge from multiple parameters.
Copyright   : (c) Tom Harding, 2020
License     : MIT
-}
module Data.JoinSemilattice.Class.Zipping (Zipping (..)) where

import Control.Applicative (liftA3)
import Data.Function ((&))
import Data.JoinSemilattice.Class.Mapping (Mapping)
import Data.JoinSemilattice.Defined (Defined)
import Data.JoinSemilattice.Intersect (Intersect, Intersectable)
import qualified Data.JoinSemilattice.Intersect as Intersect
import Data.Kind (Constraint, Type)
import Prelude hiding (unzip3)

-- | Lift a relationship between three values over some @f@ (usually a
-- parameter type).
class Mapping f c => Zipping (f :: Type -> Type) (c :: Type -> Constraint) | f -> c where
  zipWithR :: (c x, c y, c z) => ((x, y, z) -> (x, y, z)) -> ((f x, f y, f z) -> (f x, f y, f z))

  default zipWithR :: Applicative f => ((x, y, z) -> (x, y, z)) -> ((f x, f y, f z) -> (f x, f y, f z))
  zipWithR f (xs, ys, zs) = unzip3 (liftA3 (\x y z -> f (x, y, z)) xs ys zs)

instance Zipping Defined Eq

instance Zipping Intersect Intersectable where
  zipWithR f (Intersect.toList -> xs, Intersect.toList -> ys, Intersect.toList -> zs) = do
    let ( xs', ys', zs' ) = unzip3 (liftA3 (\x y z -> f (x, y, z)) xs ys zs)
    ( Intersect.fromList xs', Intersect.fromList ys', Intersect.fromList zs' )

unzip3 :: Functor f => f (x, y, z) -> (f x, f y, f z)
unzip3 xyz
  = ( xyz & fmap \(x, _, _) -> x
    , xyz & fmap \(_, y, _) -> y
    , xyz & fmap \(_, _, z) -> z
    )
