# 🔌 Wiring predicates

<!--
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
module Predicates where

import Data.Holmes
import Test.Hspec

main :: Spec
main = pure () -- do
--   describe "C2F" do
--     it "Runs forwards" c2f
--     it "Runs backwards" f2c
```
-->

?> Education never ends, Watson. It is a series of **lessons**, with the
greatest for the last.

When we use **functions** in regular programming, they look something like
this:

```haskell
add :: Int -> Int -> Int
add x y = x + y
```

Informally, we interpret this as a function from **two `Int` input** values to
**one `Int` output** value. With `Holmes` however, we're not restricted to the
linear, **input-to-output** model of computation. Knowledge can flow in all
directions, and so we replace the idea of **functions** with **relations**.

In this model, `add` should be seen as a _relationship_ between **three**
values, which establishes not only that `z = x + y`, but also that `x = z - y`
and `y = z - x`. The advantage here is that it doesn't matter which values we
learn _first_; the output can be computed before the input! Once we know any
**two** of the values, the other can be computed.

To save the user the pains of defining all these relationships manually, we use
the library's `Prop m x` abstraction to represent a propagator network
**resulting** in a value of `x` whose **computations** run in `m`. Let's look
at a classic example: conversion from **celsius** to **fahrenheit**.

## 🌡 Getting a temperature

```haskell
celsius2fahrenheit :: Prop Holmes (Defined Double) -> Prop Holmes (Defined Double)
celsius2fahrenheit x = x .* (9.0 ./ 5.0) .+ 32.0
```

Although it looks very similar to the code we would have written before, I
promise there's more going **behind the scenes**. As a sneaky peek into the
future, we can prove it using two functions: `forward` and `backward`.

```haskell
celsius :: Defined Double
celsius = 5.0

fahrenheit :: Defined Double
fahrenheit = 41.0

-- c2f = forward  celsius2fahrenheit celsius `shouldBe` Just fahrenheit
-- f2c = backward celsius2fahrenheit fahrenheit `shouldBe` Just celsius
```

This **blew my mind** when I first saw it: the propagator abstraction allows us
to run functions **backwards**! Of course, what's really going on is that our
fancy operators (`.*`, `./`, `.+`) are doing a bit more than their usual
counterparts (`*`, `/`, `+`). Specifically, they're establishing the
**relationship** between the parameters and their output. At that point, the
propagator network doesn't really care _where_ information is added, as it can
move in any direction.

In this world, **input** and **output** are just as **ordinary** as each other!

!> Of course, not all functions are **injective** in this way: sometimes, we
can't work backwards to figure out the input. However, we do the best we can:
for example, the `abs` function over `Intersect` will propagate backwards: it
just creates an `Intersect` with **two** possible inputs!

## ⛏ The raw materials

`Holmes` provides its own "upgraded" version of many of the operators we know
and love. The central theme with all these operators is that, where possible,
we try to propagate information in all directions.

| Regular | Propagator |
| --:|:-- |
| `&&` | `.&&` |
| `.%.` | `mod` |
| `./.` | `div` |
| `./`  | `/` |
| `.*.` | `*` (for `Integral` types) |
| `.*` | `*` (for `Fractional` types) |
| `.+` | `+` |
| `.-` | `-` |
| `./` | `/` |
| `./.` | `/.` |
| `./=` | `/=` |
| `.<` | `<` |
| `.<=` | `<=` |
| `.==` | `==` |
| `.>` | `>` |
| `.>=` | `>=` |
| `.\|\|` | `\|\|` |
| `abs'` | `abs` |
| `all'` | `all` |
| `and'` | `and` |
| `any'` | `any` |
| `false` | `False`* |
| `negate'` | `negate` |
| `not'` | `not` |
| `or'` | `or` |
| `recip'` | `recip` |
| `true` | `True`* |

\* _These are actually slightly more polymorphic equivalents: they represent
`True` or `False` for the given cell type, e.g. `Defined` or `Intersect`._

As one final convenience, the `distinct` function will create a predicate for
which all the values in a list must be **different** (i.e. `./=` must yield
`true`). Using these **building blocks**, we can construct any predicate we
like, and get all the benefits of propagation immediately!

## 🦁 The _even more raw_ materials

If the above operations are insufficient to produce the predicates you need to
express, see the `MonadCell` and `Prop` types (and the above operators) for
information on how to implement your own lower-level operations.

Typically, you won't need to go anywhere near these internals unless you have
something very specific in mind, though.

Now we've seen how to build predicates, why don't we [write some
`Holmes` programs](/Programs)?
