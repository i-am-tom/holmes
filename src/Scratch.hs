{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
module Scratch where

import Data.Holmes.Config (from)
import Data.Hashable (Hashable)
import Data.Holmes (satisfying)
import Data.JoinSemilattice.Defined (Defined)
import Data.JoinSemilattice.Intersect (Intersect, using)
import qualified Data.JoinSemilattice.Intersect as Intersect
import Data.List (transpose)
import Data.List.Split (chunksOf)
import Data.Propagator ((./=), (.-), (.>), (.&&), all', and', distinct)
import GHC.Generics (Generic)

dinesman :: IO [[ Defined Int ]]
dinesman = do
  let guesses = 5 `from` [1 .. 5]

  guesses `satisfying` \[ baker, cooper, fletcher, miller, smith ] -> and'
    [ distinct [ baker, cooper, fletcher, miller, smith ]
    , baker ./= 5
    , cooper ./= 1
    , fletcher ./= 1 .&& fletcher ./= 5
    , miller .> cooper
    , abs (smith .- fletcher) ./= 1
    , abs (fletcher .- cooper) ./= 1
    ]

data Val = One | Two | Three | Four | Five | Six | Seven | Eight | Nine
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable)

instance Num Val where
  fromInteger = toEnum . pred . fromInteger

  (+)    = undefined
  (*)    = undefined
  abs    = undefined
  signum = undefined
  negate = undefined

sudoku :: IO [[ Intersect Val ]]
sudoku = do
  let initial = using
          [ x, 5, 6,   x, x, 3,   x, x, x
          , 8, 1, x,   x, x, x,   x, x, x
          , x, x, x,   5, 4, x,   x, x, x
            -----------------------------
          , x, x, 4,   x, x, x,   x, 8, 2
          , 6, x, 8,   2, x, 4,   3, x, 7
          , 7, 2, x,   x, x, x,   4, x, x
            -----------------------------
          , x, x, x,   x, 7, 8,   x, x, x
          , x, x, x,   x, x, x,   x, 9, 3
          , x, x, x,   3, x, x,   8, 2, x
          ]
        where x = Intersect.fromList [1 .. 9]

  initial `satisfying` \board -> do
    let rows       = chunksOf 9 board
        columns    = transpose rows
        subsquares = do
          x <- [0 .. 2]
          y <- [0 .. 2]

          let subrows = take 3 (drop (y * 3) rows)
          pure (foldMap (take 3 . drop (x * 3)) subrows)

    all' distinct (columns <> rows <> subsquares)

-- solution :: [[Intersect Int]]
-- solution
--   = [ [ 4, 5, 6,   1, 8, 3,   2, 7, 9
--       , 8, 1, 2,   6, 9, 7,   5, 3, 4
--       , 3, 7, 9,   5, 4, 2,   6, 1, 8
-- 
--       , 1, 3, 4,   7, 6, 5,   9, 8, 2
--       , 6, 9, 8,   2, 1, 4,   3, 5, 7
--       , 7, 2, 5,   8, 3, 9,   4, 6, 1
-- 
--       , 2, 6, 3,   9, 7, 8,   1, 4, 5
--       , 5, 8, 1,   4, 2, 6,   7, 9, 3
--       , 9, 4, 7,   3, 5, 1,   8, 2, 6
--       ]
--     ]

main :: IO ()
main = sudoku >>= print
