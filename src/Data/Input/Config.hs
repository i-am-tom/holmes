{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Data.Input.Config
Description : Configuration for input parameters.
Copyright   : (c) Tom Harding, 2020
License     : MIT

Simplistically, search problems are solved by running the computation with
different input combinations, looking for any combinations that satisfy the
constraints. In reality, we play some tricks to avoid running every possible
input combination, but the principle is the same:

This module exposes the 'Config' type, which stores an initial assignment for
the input parameters (typically something close to 'mempty'), and a function
that generates possible refinements for those inputs.

For example, we might have a variable we know must be a number between @1@ and
@10@. A good initial value for this might be a 'mempty' value such as
'Data.JoinSemilattice.Defined.Unknown', with the refinements being 'Exactly'
the ten possible values.

The initial values are first fed into the computation /before/ the propagators
are established. Sometimes, these initial propagators can produce new
information (such as advancing a few steps forward in a sudoku puzzle) before
we even start to refine the inputs. The benefit here is that we can sometimes
discover that a variable's search space is smaller than we realise, and so we
end up with much less work to do!
-}
module Data.Input.Config where

import Control.Applicative (liftA2)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import Data.Kind (Type)

-- | An input configuration.
--
-- This stores both an 'initial' configuration of input parameters, as well as
-- a function that can look for ways to 'refine' an input. In other words, if
-- the initial value is an "Data.JoinSemilattice.Intersect" of @[1 .. 5]@, the
-- refinements might be 'Data.JoinSemilattice.Intersect.singleton' values of
-- every remaining possibility.
data Config (m :: Type -> Type) (x :: Type)
  = Config { initial :: [ x ], refine  :: x -> m [ x ] }

-- | The simplest way of generating an input configuration is to say that a
-- problem has @m@ variables that will all be one of @n@ possible values. For
-- example, a sudoku board is @81@ variables of @9@ possible values. This class
-- allows us to generate these simple input configurations like a game of
-- countdown: "@81@ from @1 .. 9@, please, Carol!"
class Input (x :: Type) where

  -- | Different parameter types will have different representations for their
  -- values. The 'Raw' type means that I can say @81 `from` [1 .. 9]@, and have
  -- the parameter type determine how it will represent @1@, for example. It's
  -- a little bit of syntactic sugar for the benefit of the user, so they don't
  -- need to know as much about how the parameter types work to use the
  -- library.
  type Raw x :: Type

  -- | Generate @m@ variables who are one of @n@ values. @81 `from` [1 .. 9]@,
  -- @5 `from` [ True, False ]@, and so on.
  from :: Applicative m => Int -> [ Raw x ] -> Config m x 

-- | For debugging purposes, produce a 'HashSet' of all possible refinements
-- that a 'Config' might produce for a given problem. This set could
-- potentially be very large!
permute :: (Applicative m, Eq x, Hashable x) => Config m x -> m (HashSet [ x ])
permute Config{..} = fmap HashSet.fromList (go initial)
  where go [      ] = pure [ [] ]
        go (i : is) = liftA2 (liftA2 (:)) (refine i) (go is)
