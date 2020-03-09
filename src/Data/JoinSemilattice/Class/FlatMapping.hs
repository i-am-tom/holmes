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

-- | Some types, such as `Intersect`, contain multiple "candidate values". This
-- function allows us to take /each/ candidate, apply a function, and then
-- union all the results. Perhaps @fanOut@ would have been a better name for
-- this function, but we use `(>>=)` to lend an intuition when we lift this
-- into `Prop` via `(Data.Propagator..>>=)`.
--
-- There's not normally much reverse-flow information here, sadly, as it
-- typically requires us to have a way to generate an "empty candidate" a la
-- 'mempty'. It's quite hard to articulate this in a succinct way, but try
-- implementing the reverse flow for 'Defined' or 'Intersect', and see what
-- happens.
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
