{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module      : Data.CDCL
Description : Conflict-directed clause learning utilities.
Copyright   : (c) Tom Harding, 2020
License     : MIT

Each parameter in a computation has a unique identifier, which we refer to as
its 'Major' index. Each possible /value/ of a parameter is also assigned a
unique identifier, which we refer to as its 'Minor' index.

When a conflict arises in a computation, the cause of the conflict can be
identified by a set of @('Major', 'Minor')@ pairs. Then, every branch that
involves those parameters set to /those/ values can be eliminated, as we know
they'll eventually result in a conflict.*

This module takes the conflicts we encounter, and tries to generalise them to
eliminate as many redundant branches as possible.

* In practice, we don't do this exactly. Instead, we run every branch until we
spot a cell with a previously-identified "no good" provenance. This means we
don't have to enumerate all the possible branches up front, which could end up
costing us a lot of time for no reason.
-}
module Data.CDCL where

import Control.Monad (guard)
import Data.Bifunctor (first)
import Data.Function ((&))
import Data.Functor (($>))
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (mapMaybe)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

-- | The index of a parameter in our computation.
type Major = Int

-- | The index of the chosen /value/ of a parameter in our computation.
type Minor = Int

-- | A set of value identifiers and their settings.
newtype Rule
  = Rule { toHashMap :: HashMap (Major, Minor) Bool }
  deriving newtype (Hashable, Monoid, Semigroup)
  deriving stock (Eq, Show)

-- | Generate unique rules for a set of possible values for a given parameter.
-- For example, if we assign parameter @#1@ possible values @[1 .. 4]@, this
-- function might generate something like:
--
-- @
-- [ ( -(1, 0) && -(1, 1), 1 )
-- , ( -(1, 0) &&  (1, 1), 2 )
-- , (  (1, 1) && -(1, 1), 3 )
-- , (  (1, 1) &&  (1, 1), 4 )
-- ]
-- @
index :: Major -> [ x ] -> [( Rule, x )]
index major items = map (first rulify) (go items)
  where
    rulify = Rule . HashMap.fromList . zipWith zipper [0 ..]
    zipper minor value = ((major, minor), value)

    go :: [ x ] -> [( [Bool], x )]
    go = \case
      [ ] -> []
      [x] -> pure (mempty, x)

      xs@(length -> count) -> do
        let (go -> true, go -> false) = splitAt (count `div` 2) xs
        map (first (True :)) true <> map (first (False :)) false

-- | List all the @(Major, Minor)@ pairs in a 'Rule'.
variables :: Rule -> [(Major, Minor)]
variables = HashMap.keys . toHashMap

-- | Toggle the boolean switch of a @(Major, Minor)@ pair.
invert :: (Major, Minor) -> Rule -> Rule
invert key = Rule . HashMap.update (Just . not) key . toHashMap

-- | Remove a @(Major, Minor)@ pair from a 'Rule'.
remove :: (Major, Minor) -> Rule -> Rule
remove key = Rule . HashMap.delete key . toHashMap

-- | A set of rules. We use this to represent our global list of "no good"
-- configurations. If any cell's provenance ever contains one of the rules in
-- our global set, we know this computation will eventually end in failure.
newtype Group
  = Group { toSet :: HashSet Rule }
  deriving newtype (Monoid)

instance Semigroup Group where
  Group these <> Group those
    = foldr resolve mempty (these <> those)

-- | If a group implies @(A && B)@ /and/ @(A && -B)@ then the @B@ seems to be
-- irrelevant, so we can refine the 'Rule' to @A@. This hopefully means we can
-- eliminate /more/ branches, and get to an answer faster!
refinements :: Rule -> Group -> [Rule]
refinements rule group = variables rule & mapMaybe \key ->
  guard (group `implies` invert key rule) $> remove key rule

-- | Does any 'Rule' in this 'Group' subsume the given 'Rule'?
implies :: Group -> Rule -> Bool
implies (Group group) candidate = any (`subsumes` candidate) group

-- | If @x@ 'subsumes' @y@, then the set of switches in @x@ is a strict
-- __subset__ of the switches in @y@. In other words, the @x@ 'Rule' will match
-- /everything/ that @y@ will.
subsumes :: Rule -> Rule -> Bool
subsumes (Rule these) (Rule those) = HashMap.foldrWithKey check True these
  where check key value acc = HashMap.lookup key those == Just value && acc

-- | Add a new 'Rule' to a 'Group'. Attempt to calculate any 'refinements' of
-- the rule, and generalise the 'Group' as far as possible.
resolve :: Rule -> Group -> Group
resolve rule group | group `implies` rule = group
resolve rule@(Rule config) group@(Group rules)
  = case refinements rule group of
      [] -> Group case HashMap.toList config of
        [ (key, value) ] -> do -- Unit propagation
          HashSet.insert rule $ rules & HashSet.map \(Rule current) -> do
            if HashMap.lookup key current /= Just value
              then Rule (HashMap.delete key current)
              else rule

        _ -> rules & HashSet.filter (not . (rule `subsumes`))
                   & HashSet.insert rule

      better -> foldr resolve group better
