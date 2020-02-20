# 🔢 Futoshiki

<!--
```haskell
{-# OPTIONS_GHC -Wno-missing-methods #-}
```
-->

?> The **game** is afoot.

## 📏 The rules

Futoshiki is one of my favourite **number games**. If you're unfamiliar with
the rules, we'll use the following configuration for this example:

```
┌───┐   ┌───┐   ┌───┐   ┌───┐
│   │   │   │ < │   │ < │   │
└───┘   └───┘   └───┘   └───┘
  ^
┌───┐   ┌───┐   ┌───┐   ┌───┐
│   │   │   │   │   │   │ 3 │
└───┘   └───┘   └───┘   └───┘
          v
┌───┐   ┌───┐   ┌───┐   ┌───┐
│   │   │   │   │   │   │   │
└───┘   └───┘   └───┘   └───┘
                          ^
┌───┐   ┌───┐   ┌───┐   ┌───┐
│   │   │   │   │   │   │   │
└───┘   └───┘   └───┘   └───┘
```

The goal is to fill a four-by-four board with numbers `[1 .. 4]` such that
every number is **unique** in its **row** and **column**. As well as that, if
a `<` symbol appears between two cells, the right cell must be **greater than**
the left. This "greater than" symbol can appear between any two adjacent cells,
though, so we represent it using the `<`, `>`, `^`, and `v` symbols, depending
on its direction.

They're all the clues we're getting, so let's figure it out!

## 🐛 The preamble

```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
module Futoshiki where

import Data.Hashable (Hashable)
import Data.Holmes
import Data.List (transpose)
import Data.List.Split (chunksOf)
import GHC.Generics (Generic)
import Test.Hspec
```

The `Data.Holmes` import is the most useful one here. It should be noted that
none of these language extensions are **required** to use the library; they
just either remove some boilerplate or tidy up some syntax.

When we talked about [parameters](/Parameters), we talked about `Intersect`,
and how we should define our problem space in a finite enum (_to avoid trying to
build `mempty :: Intersect Int` - a set of **all** possible `Int` values_), so
let's do that next! We know every cell will be a number between `1` and `4`:

```haskell
data Choice = V1 | V2 | V3 | V4
  deriving stock (Eq, Ord, Show, Bounded, Enum, Generic)
  deriving anyclass (Hashable)

instance Num Choice where
  fromInteger = toEnum . pred . fromInteger
```

For brevity of the example, the `Num` instance has missing methods, but
_enough_ implementation for us to use the **numeric literal overloading**, and
make the demo a bit more presentable. With all that out the way, let's define
our constraints!

## 🌿 The amble

There are `16` squares, so we'll have `16` inputs. Note that we could define
the initial board with any pre-filled squares with `using`, but below I've just
added the pre-filled `3` as an extra constraint instead.

```haskell
squares :: Applicative m => Config m (Intersect Choice)
squares = 16 `from` [1 .. 4]
```

Next up is the set of **constraints** required to perform the **search**. We
must first state that rows and columns should not contain any **duplicates**
(using the `distinct` predicate), and then we can state the constraints using
the operators from the [predicates](/Predicates) guide.

```haskell
search :: IO (Maybe [ Intersect Choice ])
search = squares `satisfying` \xs -> do
  let rows = chunksOf 4 xs

  and'
    [ all' distinct rows
    , all' distinct (transpose rows)

    , (rows !! 0 !! 1) .< (rows !! 0 !! 2)
    , (rows !! 0 !! 2) .< (rows !! 0 !! 3)
    , (rows !! 0 !! 0) .< (rows !! 1 !! 0)
    , (rows !! 1 !! 3) .== 3                
    , (rows !! 2 !! 1) .< (rows !! 1 !! 1)
    , (rows !! 2 !! 3) .< (rows !! 3 !! 3)
    ]
```

Now, `Holmes` knows everything that we know. Assuming a **valid** puzzle, that
_should_ be enough to solve the problem! Indeed, I've written a little `Hspec`
to check the answer against my pen-and-paper solution, and I think we've been
successful!

```haskell
main :: Spec
main = describe "Futoshiki" do
  it "solves the puzzle" do
    result <- search

    result `shouldBe` Just
      [ 1, 2, 3, 4
      , 2, 4, 1, 3
      , 4, 3, 2, 1
      , 3, 1, 4, 2
      ]
```

## 🦋 The postamble

Here, we've seen an example of all the component parts of the library working
together to solve a constraint problem. What's **exciting** here is that we
didn't really have to apply any thought to the problem: we simply stated our
**parameter space** and our **constraints**, then waited for `Holmes` to find a
solution.
