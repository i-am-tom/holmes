# 🌊 WaveFunctionCollapse

<!--
```haskell
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
import Data.JoinSemilattice.Intersect (fromList, singleton, toList)
import Data.List (transpose)
import Data.List.Split (chunksOf)
import Data.Propagator (lift)
import GHC.Generics (Generic)
import Data.Maybe (isJust)
import Test.Hspec (Spec, describe, it, shouldBe)
```
-->

The [WaveFunctionCollapse](https://github.com/mxgmn/WaveFunctionCollapse)
algorithm is actually a specialisation of the concepts in the field of
propagators. To demonstrate, we're going to produce the same effect using the
`Intersect` type we know and love, and a very simplified form of the algorithm
to fit in one page. Let's imagine that we want to draw a **map**:

```
💦💦💦💦💦💦💦💦💦💦💦💦💦💦💦
💦💦🔅🔅🔅🔅🔅🔅🔅🔅💦💦💦💦💦
💦🔅🔅🍀🍀🍀🍀🍀🍀🔅💦💦💦💦💦
💦🔅🍀🍀🍀🍀🍀🔅🔅🔅💦🔅🔅🔅💦
💦🔅🍀🍀🍀🍀🔅🔅💦💦💦🔅🍀🔅💦
💦🔅🔅🔅🍀🍀🔅💦💦💦🔅🔅🍀🔅💦
💦💦💦🔅🍀🍀🔅🔅💦🔅🔅🍀🍀🔅💦
💦🔅🔅🔅🍀🍀🍀🔅🔅🔅🍀🍀🍀🔅💦
💦🔅🍀🍀🍀🍀🍀🍀🍀🍀🍀🍀🍀🔅💦
💦🔅🍀🍀🍀🍀🍀🍀🍀🌲🍀🍀🍀🔅💦
💦🔅🍀🍀🍀🍀🍀🍀🍀🍀🍀🍀🍀🔅💦
💦🔅🔅🔅🔅🔅🔅🔅🔅🔅🔅🔅🔅🔅💦
💦💦💦💦💦💦💦💦💦💦💦💦💦💦💦
```

Here is a **poorly-drawn** map, where tiles are either **water**, **sand**,
**grass**, or **trees**. Some imagination may be required. Now, we want to
**generate** lots of maps similar to this, perhaps as levels for a game we're
building, and we have a few rules:

- A tree must be ***surrounded* entirely by **grass**.
- A body of water must be **surrounded** by **sand**.
- The whole map must be **surrounded** by water; it's an **island**!
- Every map must have at least one **tree**.

Hmm... these sound like **constraints**!

## 🗺 The plan

As usual, we need to start with a type to describe our **parameter space**:

```haskell
data Tile = Water | Sand | Grass | Tree
  deriving stock (Eq, Ord, Bounded, Enum, Generic)
  deriving anyclass (Hashable)

instance Show Tile where
  show = \case
    Water -> "💦"
    Sand  -> "🔅"
    Grass -> "🍀"
    Tree  -> "🌲"
```

That should do! Nothing too clever. Now, let's encode our **constraints**.
We're going to say, "for a given tile, what could its neighbours be?"

```haskell
surroundings :: Tile -> Intersect Tile
surroundings = fromList . \case
  -- A tree must be entirely surrounded by grass.
  Tree -> [ Grass ]

  -- A body of water must be surrounded by sand.
  Water -> [ Sand, Water ]

  Sand  -> [ Sand, Water, Grass ]
  Grass -> [ Sand, Tree, Grass ]
```

There we have it! Now, all we have to do is say, for every tile, apply this to
all its **neighbours**. Assuming the board is a `20 × 20` surface, this means
we can find the neighbours like so:

```haskell
neighbours :: Int -> [ x ] -> [ x ]
neighbours index xs@(length -> size) = map (xs !!) valid
  where
    indices = [ index + offset | gap <- [ 1, 19, 20, 21 ]
                               , offset <- [ gap, negate gap ] ]

    valid   = filter (\i -> i >= 0 && i < size) indices
```

It's not the most _beautiful_ way to implement this function, but it will
certainly work for our purposes. Now, we need to establish our **parameters**.
We can use `from` to produce the `Config`, and then use `shuffle` to randomise
the order of refinements. This should lead to more "natural-looking" maps!

```haskell
tiles :: Config Holmes (Intersect Tile)
tiles = shuffle (400 `from` [ Water, Sand, Grass, Tree ]) -- 20 × 20 board.
```

Finally, let's put together the constraints into a full computation to find the
first valid answer. At this point, our program starts to look pretty familiar
to examples we've seen elsewhere (except maybe the use of `.>>=` to generate
possibilities from _each remaining candidate_ in an `Intersect`):

```haskell
maps :: IO (Maybe [ Intersect Tile ])
maps = do
  tiles `satisfying` \board@(chunksOf 20 -> rows) -> do
    let columns = transpose rows

    and'
      [ -- The whole map must be **surrounded** by water; it's an **island**!
        all' (.== lift (singleton Water)) (head rows)
      , all' (.== lift (singleton Water)) (last rows)

      , all' (.== lift (singleton Water)) (head columns)
      , all' (.== lift (singleton Water)) (last columns)

        -- Every map must have at least one tree.
      , any' (.== lift (singleton Tree)) board

        -- Constrain every cell's neighbours according to `surroundings`.
      , board & allWithIndex' \index tile -> do
          let candidates = tile .>>= surroundings
          all' (.== candidates) (neighbours index board)
      ]
```

That's it! If you want to try generating some maps, run `cabal new-repl
test:docs`, and then enter something like the following:

```
> import WaveFunctionCollapse
> example <- maps
> mapM_ printMap example
```

If all goes to plan, you should see something like this!

```
💦💦💦💦💦💦💦💦💦💦💦💦💦💦💦💦💦💦💦💦
💦💦🔅🔅💦🔅🔅💦🔅🔅🔅💦💦💦🔅🔅🔅💦🔅💦
💦💦💦🔅🔅🔅🔅🔅🔅🍀🔅💦🔅💦💦💦🔅💦💦💦
💦💦🔅💦🔅🔅🔅🍀🍀🔅🔅💦🔅🔅💦💦💦🔅🔅💦
💦🔅🔅🔅🔅🔅🍀🍀🔅🍀🔅💦💦💦🔅🔅🔅💦🔅💦
💦💦🔅🍀🔅🔅🔅🔅🍀🍀🔅💦💦💦💦💦🔅💦💦💦
💦💦🔅🍀🍀🔅🔅🔅🍀🔅🔅🔅🔅💦🔅🔅🔅💦💦💦
💦💦🔅🍀🍀🔅🔅🍀🔅🔅🍀🍀🔅💦🔅🔅🔅🔅🔅💦
💦💦🔅🔅🍀🍀🍀🔅🍀🍀🔅🔅🔅🔅🔅🔅🔅🔅💦💦
💦🔅🔅🍀🍀🍀🔅🍀🔅🔅🔅🔅🔅💦🔅🍀🔅🔅🔅💦
💦🔅🍀🔅🔅🔅🔅🍀🔅🔅💦💦🔅🔅🔅🍀🍀🔅💦💦
💦🔅🔅🍀🍀🍀🍀🔅🍀🔅💦💦💦💦🔅🍀🍀🔅💦💦
💦💦🔅🔅🍀🌲🍀🍀🍀🔅🔅💦💦🔅🔅🍀🔅🔅💦💦
💦💦🔅🔅🍀🍀🍀🍀🔅🍀🔅🔅💦💦🔅🔅🍀🔅🔅💦
💦🔅💦🔅🔅🍀🍀🍀🔅🔅🔅🔅🔅💦💦🔅🍀🍀🔅💦
💦🔅💦🔅🍀🔅🍀🔅🔅🍀🍀🔅💦💦🔅🔅🔅🔅🔅💦
💦🔅💦🔅🔅🔅🔅🔅🍀🍀🔅🔅💦💦🔅💦💦💦💦💦
💦💦💦💦💦🔅💦🔅🔅🔅🍀🔅🔅🔅💦🔅🔅💦💦💦
💦🔅💦🔅🔅💦💦🔅🔅🔅🔅🔅💦💦💦💦🔅🔅💦💦
💦💦💦💦💦💦💦💦💦💦💦💦💦💦💦💦💦💦💦💦
```

There we have it: a procedurally-generated **emoji map**!

<!--
```haskell
printMap :: [ Intersect Tile ] -> IO ()
printMap xs = mapM_ (putStrLn . foldMap (show . head . toList)) (chunksOf 20 xs)

main :: Spec
main = describe "WaveFunctionCollapse" do
  it "generates a map" do
    result <- maps

    isJust result `shouldBe` True
```
-->
