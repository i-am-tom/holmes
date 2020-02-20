{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module      : Data.JoinSemilattice.Intersect
Description : Solving problems by reducing lists of candidates.
Copyright   : (c) Tom Harding, 2020
License     : MIT

When we play games like Guess Who?, we start with a set of possible candidates,
and eliminate subsets of them as the game progresses. The 'Intersect' type
works in a similar way: each cell stores a list of its potential values, and
the merging operation takes the __intersect__ of the current candidates and the
new candidates.
-}
module Data.JoinSemilattice.Intersect where

import Control.Applicative (liftA2)
import Data.Coerce (coerce)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import Data.Input.Config (Config (..), Input (..))
import Data.Kind (Type)
import Prelude hiding (filter, map, unzip)

-- | A set type with intersection as the '(<>)' operation.
newtype Intersect (x :: Type)
  = Intersect { toHashSet :: HashSet x }
  deriving stock (Eq, Ord, Show, Foldable)
  deriving newtype (Hashable)

class (Bounded content, Enum content, Eq content, Hashable content)
  => Intersectable content

instance (Bounded content, Enum content, Eq content, Hashable content)
  => Intersectable content

instance (Eq content, Hashable content) => Semigroup (Intersect content) where
  (<>) = coerce HashSet.intersection

instance Intersectable content => Monoid (Intersect content) where
  mempty = fromList [ minBound .. maxBound ]

lift2
  :: ( Intersectable this
     , Intersectable that
     , Intersectable result
     )
  => (this -> that -> result)
  -> Intersect this
  -> Intersect that
  -> Intersect result

lift2 f these those = fromList do
  liftA2 f (toList these) (toList those)

instance (Intersectable content, Num content)
    => Num (Intersect content) where
  (+) = lift2 (+)
  (*) = lift2 (*)
  (-) = lift2 (-)

  abs         = map abs
  fromInteger = singleton . fromInteger
  negate      = map negate
  signum      = map signum

instance (Intersectable x, Fractional x) => Fractional (Intersect x) where
  (/) = lift2 (/)

  fromRational = singleton . fromRational
  recip = map recip

-- | Create an 'Intersect' from a list of candidates.
fromList :: (Eq x, Hashable x) => [ x ] -> Intersect x
fromList = coerce HashSet.fromList

-- | Return a list of candidates stored within an 'Intersect'.
toList :: (Bounded x, Enum x, Eq x) => Intersect x -> [ x ]
toList = coerce HashSet.toList

-- | Run an action /only if/ a single candidate remains.
decided :: (Applicative m, Intersectable x) => (x -> m ()) -> Intersect x -> m ()
decided f = \case
  (toList -> [ x ]) -> f x
  _                 -> pure ()

-- | Delete a candidate from an 'Intersect'.
delete :: Intersectable x => x -> Intersect x -> Intersect x
delete = coerce HashSet.delete

-- | Return an 'Intersect' of /all possible candidates/ except those in the
-- given 'Intersect'. The 'Intersect' of /all/ candidates is assumed to be
-- 'mempty'.
except :: Intersectable x => Intersect x -> Intersect x
except = foldr delete mempty

-- | Filter an 'Intersect' with a predicate.
filter :: (x -> Bool) -> Intersect x -> Intersect x
filter = coerce HashSet.filter

-- | Map over an 'Intersect' with a given function.
map :: (Eq y, Hashable y) => (x -> y) -> Intersect x -> Intersect y
map = coerce HashSet.map

-- | Create a singleton 'Intersect'.
singleton :: Hashable x => x -> Intersect x
singleton = coerce HashSet.singleton

-- | Count the candidates in an 'Intersect'.
size :: Intersectable x => Intersect x -> Int
size = coerce HashSet.size

-- | Merge two 'Intersect' values with set __union__.
union :: Intersectable x => Intersect x -> Intersect x -> Intersect x 
union = coerce ((<>) @(HashSet _))

instance Intersectable x => Input (Intersect x) where
  type Raw (Intersect x) = x

  from count = using . replicate count . fromList

-- | Produce a 'Config' with the given /initial/ value, where the 'refine'
-- function just tries each remaining candidate as a singleton.
using :: (Applicative m, Intersectable x) => [ Intersect x ] -> Config m (Intersect x)
using xs = Config xs (pure . fmap singleton . toList)
