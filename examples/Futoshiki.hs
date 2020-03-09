{-# OPTIONS_GHC -Wno-missing-methods #-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- Futoshiki is one of my favourite number games. If you're unfamiliar with the
-- rules, we'll use the following configuration for this example:
--
-- ┌───┐   ┌───┐   ┌───┐   ┌───┐
-- │   │   │   │ < │   │ < │   │
-- └───┘   └───┘   └───┘   └───┘
--   ^
-- ┌───┐   ┌───┐   ┌───┐   ┌───┐
-- │   │   │   │   │   │   │ 3 │
-- └───┘   └───┘   └───┘   └───┘
--           v
-- ┌───┐   ┌───┐   ┌───┐   ┌───┐
-- │   │   │   │   │   │   │   │
-- └───┘   └───┘   └───┘   └───┘
--                           ^
-- ┌───┐   ┌───┐   ┌───┐   ┌───┐
-- │   │   │   │   │   │   │   │
-- └───┘   └───┘   └───┘   └───┘
--
-- The goal is to fill a four-by-four board with numbers `[1 .. 4]` such that
-- every number is __unique__ in its __row__ and __column__. As well as that,
-- if a @<@ symbol appears between two cells, the right cell must be **greater
-- than** the left. This "greater than" symbol can appear between any two
-- adjacent cells, though, so we represent it using the @<@, @>@, @^@, and @v@
-- symbols, depending on its direction.
module Futoshiki where

import Data.Hashable (Hashable)
import Control.Monad.Watson (satisfying)
import Data.Holmes hiding (satisfying)
import Data.List (transpose)
import Data.List.Split (chunksOf)
import GHC.Generics (Generic)
import Test.Hspec

-- We'll be using @Intersect@ for this one, so we need to establish our enum
-- type for the parameter space.
data Choice = V1 | V2 | V3 | V4
  deriving stock (Eq, Ord, Show, Bounded, Enum, Generic)
  deriving anyclass (Hashable)

instance Num Choice where
  fromInteger = toEnum . pred . fromInteger

-- Here's the translation of the board shown above, with the constraints
-- expressed as a `Prop` predicate:
solution :: Maybe [ Intersect Choice ]
solution = do

  -- For this example, the board is a @4 × 4@ grid with each cell being a
  -- number between @1@ and @4@.
  (16 `from` [1 .. 4]) `satisfying` \board -> do
    let rows    = chunksOf 4 board
        columns = transpose rows

    and'
      [ -- First up, the rules of the game:
        all' distinct rows
      , all' distinct columns

        -- Then, the constraints on this particular board:
      , (rows !! 0 !! 1) .< (rows !! 0 !! 2)
      , (rows !! 0 !! 2) .< (rows !! 0 !! 3)
      , (rows !! 0 !! 0) .< (rows !! 1 !! 0)
      , (rows !! 1 !! 3) .== 3                
      , (rows !! 2 !! 1) .< (rows !! 1 !! 1)
      , (rows !! 2 !! 3) .< (rows !! 3 !! 3)
      ]

-- All being well, this should be the result! Use `cabal new-test examples` to
-- run these tests and check for correct solutions.

spec_futoshiki
  = it "computes the solution" do
      solution `shouldBe` Just
        [   1,   2,   3,   4

        ,   2,   4,   1,   3

        ,   4,   3,   2,   1

        ,   3,   1,   4,   2
        ]
