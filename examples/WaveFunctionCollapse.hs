{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module WaveFunctionCollapse where

import Data.Function ((&))
import Data.Hashable (Hashable)
import Data.Holmes
import Data.JoinSemilattice.Intersect (fromList, toList)
import Data.List (transpose)
import Data.List.Split (chunksOf)
import Data.Maybe (isJust, mapMaybe)
import GHC.Generics (Generic)
import Relude ((!!?))
import Test.Hspec (Spec, it, shouldBe)

-- Wave function collapse* is an algorithm that works by placing constraints
-- between each cell and their neighbours. A cell is randomly specialised to a
-- particular value, and the effects ripple out via the constraints. Then,
-- another cell is specialised, and the process repeats until all cells are
-- specialised.
--
-- It turns out that this is actually just a special case of the propagator
-- idea, and specifically the `Intersect` strategy. While we're not going to
-- implement the full algorithm here, we'll demonstrate the idea with a
-- simplified version in order to draw some desert island maps!
--
-- * https://github.com/mxgmn/WaveFunctionCollapse

--------------------------------------------------

-- First, we'll start with a type to specify the possible terrain types in our
-- map:
data Tile = Water | Sand | Grass | Tree
  deriving stock (Eq, Ord, Bounded, Enum, Generic)
  deriving anyclass (Hashable)

instance Show Tile where
  show = \case
    Water -> "💦"
    Sand  -> "🔅"
    Grass -> "🍀"
    Tree  -> "🌲"

-- Now, we'll specify some constraints on our neighbours. Again, this is a very
-- simplified version of the WaveFunctionCollapse concept - typically, we'd
-- have far more "tiles", and neighbours would be chosen by properties attached
-- to each edge of each tile.

surroundings :: Tile -> Intersect Tile
surroundings = fromList . \case

  -- A tree must be entirely surrounded by grass. Two trees cannot touch, and
  -- trees cannot be on beaches or in water.
  Tree -> [ Grass ]

  -- The only thing that can neighbour water is more water or sand. This means
  -- that every island has a beach, and we might even get some small islands
  -- out in water, too!
  Water -> [ Sand, Water ]

  -- Sand must sit between water and grass. Note that this simple system
  -- doesn't prevent random sand tiles amid grass; we'd need to specify the
  -- constraints in a more comprehensive way to mitigate this.
  Sand -> [ Sand, Water, Grass ]

  -- Grass can neighbour sand, more grass, or trees!
  Grass -> [ Sand, Tree, Grass ]

-- Get the neighbours of a cell at a given index.
neighbours :: Int -> [ x ] -> [ x ]
neighbours index board = mapMaybe (board !!?)
  [ index - 21, index - 20, index - 19
  , index -  1, {- HOME! -} index +  1
  , index + 19, index + 20, index + 21
  ]

-- The 20 × 20 board makes up 400 tiles.
tiles :: Config Holmes (Intersect Tile)
tiles = shuffle (400 `from` [ Water, Sand, Grass, Tree ])

--------------------------------------------------

maps :: IO (Maybe [ Intersect Tile ])
maps = do
  tiles `satisfying` \board@(chunksOf 20 -> rows) -> do
    let columns = transpose rows

    and'
      [ -- As we're trying to draw an island, we'll surround the whole map with
        -- water:
        all' (.== lift Water) (head rows)
      , all' (.== lift Water) (last rows)

      , all' (.== lift Water) (head columns)
      , all' (.== lift Water) (last columns)

        -- To generate more interesting maps, we'll require that every valid
        -- map contains at least one tree (and thus has at least one 5 × 5
        -- island).
      , any' (.== lift Tree) board

        -- For each tile, find the valid surrounding tiles, then constraint its
        -- neighbours to those possibilities.
      , board & allWithIndex' \index tile -> do
          let candidates = tile .>>= surroundings
          all' (.== candidates) (neighbours index board)
      ]

-- If you want to see some of the generated maps, run `cabal new-repl examples`
-- and use the following function to print out a result:
--
-- > import WaveFunctionCollapse
-- > Just example <- maps
-- > printMap example

printMap :: [ Intersect Tile ] -> IO ()
printMap (chunksOf 20 -> rows) = mapM_ printRow rows
  where printRow = putStrLn . foldMap (show . head . toList)

-- Use `cabal new-test examples` to run these tests and check for correct
-- solutions.

spec_wfc :: Spec
spec_wfc = it "generates a map" do
  maps >>= \result -> isJust result `shouldBe` True
