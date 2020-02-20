# 🛠 Constructing parameters

?> “Excellent!" I cried. "**Elementary**," said he.”

<!--
```haskell
{-# OPTIONS_GHC -Wno-missing-methods #-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
module Parameters where

import Data.JoinSemilattice.Intersect (fromList, using)
import Data.Hashable (Hashable)
import qualified Data.HashSet as HashSet
import Data.Holmes
import GHC.Generics (Generic)
import Test.Hspec

main :: Spec
main = do
  describe "Defined" do
    it "produces appropriate config" defined_permute

  describe "Intersect" do
    it "mappends correctly" intersect_mappend
```
-->

When we have a problem to solve within `Holmes`, we start by constructing a set
of input **parameters**. These represent the **initial** values that we'll feed
into the computation.

## 🎯 Well-`Defined` Problems

When we lift our familiar computations up into the world of `Holmes`, the
`Defined` type is often the most intuitive choice for a parameter type. The
type itself looks something like this:

```hs
data Defined (x :: Type) = Unknown | Exactly x | Conflict
```

In the world of `Defined`, there are only three types of values:

- Those with **many** possible values, so the answer is `Unknown` until someone
  gives us some more information.

- Those with **one** possible value, for which we know `Exactly` what the
  answer is.

- Those with **no** possible values, which suggests a `Conflict` between
  sources somewhere in our computation.

This is a nice and simple view of knowledge, and is a great place to start when
getting comfortable with `Holmes`. The library exposes the `from` function to
generate sets of `Defined` inputs:

```haskell
three_bools :: Applicative m => Config m (Defined Bool)
three_bools = 3 `from` [ False, True ]
```

`three_bools` is a configuration that says we're going to have three `Bool`
input variables set to either `False` or `True`. This means we'll have a
_maximum_ of **eight** computation branches to run in order to explore the
search space. We'll see later how we can often cut down the search space
significantly with some clever use of **propagators**.

?> For those who've ever seen Countdown, I read this very much as a contestant
would: __4 from the top, please, Carol!__

If you ever want to see the branches that a given `Config` will try, use the
`permute` function:

```haskell
defined_permute :: IO ()
defined_permute = do
  permutations <- permute three_bools
  
  permutations `shouldBe` HashSet.fromList
    [ [ Exactly False, Exactly False, Exactly False ]
    , [ Exactly False, Exactly False, Exactly True  ]
    , [ Exactly False, Exactly True,  Exactly False ]
    , [ Exactly False, Exactly True,  Exactly True  ]
    , [ Exactly True,  Exactly False, Exactly False ]
    , [ Exactly True,  Exactly False, Exactly True  ]
    , [ Exactly True,  Exactly True,  Exactly False ]
    , [ Exactly True,  Exactly True,  Exactly True  ]
    ]
```

## 🧗‍♀️ Exhausting ideas with `Intersect`

`Defined` is a good start, and certainly a good fit for lots of problems.
However, we can often get some **performance** wins using `Intersect` depending
on the problem in question.

Imagine a **sudoku board** with a row with one unfilled cell. If we know the
rules of sudoku, we can figure out the missing value: it's the only number
between `1` and `9` that doesn't already appear in the row!

Sadly, our `Defined` type can't do this sort of reasoning, because it doesn't
have a way of checking **exhaustivity**. What that means in _this_ case is that
it doesn't keep track of what a value _can't_ be, and hence doesn't have an
idea of what it _can_ be (it's just `Unknown`!) Ideally, if there could only be
one possible value to fill a given variable, we should just pick that value!

We'd probably construct a sudoku board using a config such as this:

```haskell
data Value = V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9
  deriving stock (Eq, Ord, Show, Bounded, Enum, Generic)
  deriving anyclass (Hashable) -- Look, Watson: a clue!

instance Num Value where
  fromInteger = toEnum . pred . fromInteger

defined_sudoku :: Applicative m => Config m (Defined Value)
defined_sudoku = 81 `from` [1 .. 9]
```

This is _fine_, but sudoku relies on keeping track of the possible
**candidates** for each square! If we can't do _that_, we have to try **every
configuration** and hope we find the correct one eventually.

Thankfully, `Holmes` is a bit cleverer. For these sorts of problems, we have
the `Intersect` type. `Intersect` is identical to the `Set` or (more
specifically) `HashSet` types we may already know, but with one important
difference: `mappend` is set **intersection**:

<!--
```haskell
intersect_mappend :: Expectation
```
-->

```haskell
intersect_mappend = do
  let this :: Intersect Value
      this = fromList [1 .. 5]

      that :: Intersect Value
      that = fromList [3 .. 8]

  this <> that `shouldBe` fromList [3 .. 5]
```

In other words, when we join two `Intersect` values together, we keep only the
items that appear in **both**. We can use this to implement **exhaustivity**:
if the sudoku squares, upon figuring out their specific value, tell every
neighbour that they can be anything _except_ that value, then every cell will
slowly whittle down their `9` possibilities to a single one!

Now, how to create `Config` with `Intersect`? Luckily, it turns out that `from`
is more **polymorphic** than we might initially think:

```haskell
intersect_sudoku :: Applicative m => Config m (Intersect Value)
intersect_sudoku = 81 `from` [1 .. 9]
```

!> You might be wondering why we don't just use `Intersect Int`? The reason is
that the `mempty` for `Intersect` is the set of all possible values of that
type. For `Int`, that's a pretty big set! Often, propagator networks will
create intermediate values to maximise information sharing, and their initial
values will be `mempty`. Creating a finite enum like `Value` saves us from
**nasty run-time surprises** when your program tries to build that set!

There we have it! There's one extra function to cover when we talk about
`Intersect`, and that's `using`. This function lets us set up more specific
initial states, which is perfect for something like sudoku, or even more
advanced applications like
[WaveFunctionCollapse](https://github.com/mxgmn/WaveFunctionCollapse). For
example, we could specify our initial sudoku board up front like so:

```haskell
intersect_board :: Applicative m => Config m (Intersect Value)
intersect_board = let x = mempty in using
    [ x, 5, 6,   x, x, 3,   x, x, x
    , 8, 1, x,   x, x, x,   x, x, x
    , x, x, x,   5, 4, x,   x, x, x

    , x, x, 4,   x, x, x,   x, 8, 2
    , 6, x, 8,   2, x, 4,   3, x, 7
    , 7, 2, x,   x, x, x,   4, x, x

    , x, x, x,   x, 7, 8,   x, x, x
    , x, x, x,   x, x, x,   x, 9, 3
    , x, x, x,   3, x, x,   8, 2, x
    ]
```

## 🌱 Sowing the seeds

The first step to understanding a `Holmes` computation is understanding the
space you intend to search. Usually, our parameters' possible values are
enumerable, and this is the kind of search for which `Holmes` excels.

Of course, there are exceptions: not all problems fit quite so neatly into this
model. When this happens, `Holmes` gives us two options:

1. Build your own `Config` object! `Data.Holmes` exports the constructor, so
   feel free to see how the library's types implement `from` and go from there!

2. Not all computations involve **searching** a space. We'll see later that,
  for simple problems, such as solving mathematical **equations**, we don't
  need any sort of search space, and a careful choice of type can save the day.

Hopefully, the above gives a good idea about how we construct parameter spaces
to search. Once you're familiar with the `from` function (or at least
understand roughly how it works), we'll move on to [how to specify our
constraints as predicates](/Predicates).
