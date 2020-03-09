{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}

{-|
Module      : Data.JoinSemilattice.Class.FlatMapping
Description : Refine parameters using their raw values.
Copyright   : (c) Tom Harding, 2020
License     : MIT
-}
module Data.JoinSemilattice.Class.FlatMapping where

import Data.JoinSemilattice.Class.Zipping (Zipping)
import Data.JoinSemilattice.Defined (Defined (..))
import Data.JoinSemilattice.Intersect (Intersect (..), Intersectable)
import Data.Kind (Constraint, Type)
import Prelude hiding (unzip)

-- | Flat mapping allows us to inspect a current candidate value to try to
-- refine it further. For example, in the wave function collapse example, we
-- inspect a value to calculate all its possible surrounding tiles.
class Zipping f c => FlatMapping (f :: Type -> Type) (c :: Type -> Constraint) | f -> c where
  flatMapR :: (c x, c y) => ((x, f y) -> (x, f y)) -> ((f x, f y) -> (f x, f y))

instance FlatMapping Defined Eq where
  flatMapR f ( xs, _ )
    = ( mempty -- Unless you have 'Monoid x'
      , case xs of Exactly x -> let ( _, ys' ) = f (x, mempty) in ys'
                   _         -> mempty
      )

instance FlatMapping Intersect Intersectable where
  flatMapR f ( Intersect xs, _ )
    = ( mempty -- Unless you have 'Monoid x'
        
        -- Take the union of all generated 'Intersect' values.
      , Intersect (foldMap (\x -> let (_, Intersect ys') = f (x, mempty) in ys') xs)
      )
