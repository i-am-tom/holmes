# 🕵️‍♂️ Holmes

**Holmes** is a library for computing **constraint-solving** problems. Under
the hood, it uses **propagator networks** and **conflict-directed clause
learning** to optimise the search over the parameter space.

Now available on [Hackage](https://hackage.haskell.org/package/holmes)!

<!--

```haskell
{-# OPTIONS_GHC -Wno-missing-methods -Wno-unused-top-binds #-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}

import Data.List (transpose)
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Test.Hspec (describe, hspec, it, shouldBe)
```

-->

## 👟 Example

[Dinesman's
problem](https://rosettacode.org/wiki/Dinesman%27s_multiple-dwelling_problem)
is a nice first example of a constraint problem. In this problem, we imagine
**five** people — Baker, Cooper, Fletcher, Miller, and Smith —  living in a
five-story apartment block, and we must figure out the floor on which each
person lives. Here's how we state the problem with `Holmes`:

```haskell
import Data.Holmes

dinesman :: IO (Maybe [ Defined Int ])
dinesman = do
  let guesses = 5 `from` [1 .. 5]

  guesses `satisfying` \[ baker, cooper, fletcher, miller, smith ] -> and'
    [ distinct [ baker, cooper, fletcher, miller, smith ]
    , baker ./= 5
    , cooper ./= 1
    , fletcher ./= 1 .&& fletcher ./= 5
    , miller .> cooper
    , abs' (smith .- fletcher) ./= 1
    , abs' (fletcher .- cooper) ./= 1
    ]
```

## 👣 Step-by-step problem-solving

Now we've written the poster example, how do we go about **stating** and
**solving** our own constraint problems?

### ⚖️ 0. Pick a parameter type

Right now, there are **two** parameter type constructors: `Defined` and
`Intersect`. The choice of type determines the **strategy** by which we solve
the problem:

- `Defined` only permits two levels of knowledge about a value: **nothing** and
  **everything**. In other words, it doesn't support a notion of _partial_
  information; we either know a value, or we don't. This is fine for small
  problem spaces, particularly when few branches are likely to fail, but
  we can usually achieve faster results using another type.

- `Intersect` stores a set of "possible answers", and attempts to eliminate
  possibilities as the computation progresses. For problems with many
  constraints, this will produce **significantly faster** results than
  `Defined` as we can hopefully discover failures much earlier.

It would seem that `Intersect` would be the best choice in most cases, but
beware: it will only work for **small** enum types. An `Intersect Int` for
which we have no knowledge will contain every possible `Int`, and will
therefore take an **intractable** time to compute. `Defined` has no such
restrictions.

### 🗺 1. State the parameter space

Next, we need to produce a `Config` stating the search space we want to explore
when looking for satisfactory inputs. The simplest way to do this is with the
`from` function:

```hs
from :: Int -> [ x ] -> Config Holmes (Defined x)
```

```hs
from :: Int -> [ x ] -> Config Holmes (Intersect x)
```

If, for example, we wanted to solve a Sudoku problem, we might say something
like this:

```haskell
definedConfig :: Config Holmes (Defined Int)
definedConfig = 81 `from` [ 1 .. 9 ]
```

We read this as, "`81` variables whose values must all be numbers between `1`
and `9`". At this point, we place no constraints (such as uniqueness of rows or
columns); we're just stating the possible range of values that could exist in
each parameter.

We could do the same for `Intersect`, but we'd first need to produce some
**enum** type to represent our parameter space:

```haskell
data Value = V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable)

instance Num Value where -- Syntactic sugar for numeric literals.
  fromInteger = toEnum . pred . fromInteger
```

_Now_, we can produce an `Intersect` parameter space. Because we can now work
with a type who has only `9` values, rather than all possible `Int` values,
producing the initial possibilities list becomes tractable:

```haskell
intersectConfig :: Config Holmes (Intersect Value)
intersectConfig = 81 `from` [ 1 .. 9 ]
```

There's one more function that lets us do slightly better with an `Intersect`
strategy, and that's `using`:

```hs
using :: [ Intersect Value ] -> Config Holmes (Intersect Value)
```

With `using`, we can give a precise "initial state" for all the `Intersect`
variables in our system. This, it turns out, is very convenient when we're
trying to state sudoku problems:

```haskell
squares :: Config Holmes (Intersect Value)
squares = let x = mempty in using
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

Now, let's write some **constraints**!

### 📯 2. Declare your constraints

Typically, your constraints should be stated as a **predicate** on the input
**parameters**, with a type that, when specialised to your problem, should look
something like `[Prop Holmes (Defined Int)] -> Prop Holmes (Defined Bool)`.
Now, what's this `Prop` type?

#### 🕸 Propagators

If this library has done its job properly, this predicate shouldn't look too
dissimilar to regular predicates. However, behind the scenes, the `Prop` type
is wiring up a lot of **relationships**.

As an example, consider the `(+)` function. This has two inputs and one output,
and the output is the sum of the two inputs. This is totally fixed, and there's
nothing we can do about it. This is fine when we write normal programs, because
we only have **one-way information flow**: input flows to output, and it's as
simple as that.

When we come to constraint problems, however, we have **multi-way information
flow**: we might know the output before we know the inputs! Ideally, it'd be
nice in these situations if we could "work backwards" to the information we're
missing.

When we say `x .+ y .== z`, we actually wire up **multiple** relationships:
`x + y = z`, `z - y = x`, and `z - x = y`. That way, as soon as we learn
**two** of the three values involved in addition, we can infer the other!

The operators provided by this library aim to **maximise** information flow
around a propagator network by automatically wiring up all the different
**identities** for all the different operators. We'll see later that this
allows us to write seemingly-magical functions like `backwards`: given a
function and an **output**, we can produce the function's input!

#### 🛠 The problem-solving toolkit

With all this in mind, the following functions are available to us for
multi-directional information flow. We'll leave the type signatures to Haddock,
and instead just run through the functions and either their analogous regular
functions _or_ a brief explanation of what they do:

##### 🎚 Boolean functions

| Function | Analogous function / notes |
| --:|:-- |
| `(.&&)` | `(&&)` |
| `all'` | `all` |
| `allWithIndex'` | `all'`, but the predicate _also_ receives the list index |
| `and'` | `and` |
| `(.\|\|)` | `(\|\|)` |
| `any'` | `any` |
| `anyWithIndex'` | `any'`, but the predicate _also_ receives the list index |
| `or'` | `or` |
| `not'` | `not` |
| `false` | `False` |
| `true` | `True` |

##### 🏳️‍🌈 Equality functions

| Function | Analogous function / notes |
| --:|:-- |
| `(.==)` | `(==)` |
| `(./=)` | `(/=)` |
| `distinct` | Are all list elements _different_ (according to `(./=)`)? |

##### 🥈 Comparison functions

| Function | Analogous function / notes |
| --:|:-- |
| `(.<)` | `(<)` |
| `(.<=)` | `(<=)` |
| `(.>)` | `(>)` |
| `(.>=)` | `(>=)` |

##### 🎓 Arithmetic functions

| Function | Analogous function / notes |
| --:|:-- |
| `(.*)` | `(*)` |
| `(./)` | `(/)` |
| `(.+)` | `(+)` |
| `(.-)` | `(-)` |
| `(.%.)` | `mod` |
| `(.*.)` | `(*)` for _integral_ functions |
| `(./.)` | `div` |
| `abs'` | `abs` |
| `negate'` | `negate` |
| `recip'` | `recip` |

##### 🌱 Information-generating functions

| Function | Analogous function / notes |
| --:|:-- |
| `(.$)` | Apply a function to the value _within_ the parameter type.
| `zipWith'` | Similar to `liftA2`; generate results from the parameters. |
| `(.>>=)` | Turn each value within the parameter type into the parameter type. |

The analogy gets stretched a bit here, unfortunately. It's perhaps helpful to
think of these functions in terms of `Intersect`:

- `(.$)` maps over the remaining candidates in an `Intersect`.

- `zipWith'` creates an `Intersect` of the **cartesian product** of the two
  given `Intersect`s, with the pairs applied to the given function.

- `(.>>=)` takes every remaining candidate, applies the given function, then
  **unions** the results to produce an `Intersect` of all possible results.

---

Using the above toolkit, we could express the constraints of our **sudoku**
example. After we establish some less interesting functions for splitting up
our `81` inputs into helpful chunks...

```haskell
rows :: [ x ] -> [[ x ]]
rows [] = []
rows xs = take 9 xs : rows (drop 9 xs)

columns :: [ x ] -> [[ x ]]
columns = transpose . rows

subsquares :: [ x ] -> [[ x ]]
subsquares xs = do
  x <- [ 0 .. 2 ]
  y <- [ 0 .. 2 ]

  let subrows = take 3 (drop (y * 3) (rows xs))
      values  = foldMap (take 3 . drop (x * 3)) subrows

  pure values
```

... we can use the **propagator toolkit** to specify our constraints in a
delightfully straightforward way:

```haskell
constraints :: forall m. MonadCell m => [ Prop m (Intersect Value) ] -> Prop m (Intersect Bool)
constraints board = and'
  [ all' distinct (columns    board)
  , all' distinct (rows       board)
  , all' distinct (subsquares board)
  ]
```

> _The type signature looks a little bit **ugly** here, but the polymorphism is
to guarantee that predicate computations are totally generic propagator
networks that can be run in any interpretation strategy. As we'll see later,
`Holmes` isn't the only one capable of solving a mystery!_
>
> _Typically, we write the constraint predicate inline (as we did for the
> Dinesman example above), so we never usually write this signature anyway!)_

We've explained all the rules and **constraints** of the sudoku puzzle, and
designed a propagator network to solve it! Now, why don't we get ourselves a
**solution**?

### 💡 3. Solving the puzzle

Currently, `Holmes` only exposes two strategies for solving constraint
problems:

- `satisfying`, which returns the **first** valid configuration that is found,
  **if one exists**. As soon as this result has been found, computation will
  cease, and this program will return the result.

- `whenever`, which returns **all** valid configurations in the search space.
  This function could potentially run for a long time, depending on the size of
  the search space, so you might find better results by sticking to
  `satisfying` and simply adding more constraints to eliminate the results you
  don't want!

These functions are named to be written as **infix** functions, which hopefully
makes our programs a lot easier to read:

```haskell
sudoku :: IO (Maybe [ Intersect Value ])
sudoku = squares `satisfying` constraints
```

At last, we combine the three steps to solve our problem. This README is a
**literate Haskell file** containing a **complete sudoku solver**, so feel free
to run `cabal new-test readme` and see for yourself!

## 🎁 Bonus surprises

We've now covered almost the **full API** of the library. However, there are a
couple extra little surprises in there for the curious few:

### 📖 `Control.Monad.Watson`

`Watson` knows `Holmes`' methods, and can apply them to compute results. Unlike
`Holmes`, however, `Watson` is built on top of `ST` rather than `IO`, and is
thus is a much purer soul.

Users can import `Control.Monad.Watson` and use the equivalent `satisfying` and
`whenever` functions to return results _without_ the `IO` wrapper, thus making
these computations **observably pure**! For most computations — certainly those
outlined in this README — `Watson` is more than capable of deducing results.

### 🎲 Random restart with `shuffle`

`Watson` isn't quite as capable as `Holmes`, however. Consider a typical
`Config`:

```haskell
example :: Config Holmes (Defined Int)
example = 1 `from` [1 .. 10]
```

With this `Config`, a program will run with a single parameter. For the _first_
run, that parameter will be set to `Exactly 1`. For the _second_ run, it will
be set to `Exactly 2`. In other words, it tries each value **in order**.

For many problems, however, we can get to results faster — or produce more
desirable results — by applying some **randomness** to this order. This is
especially useful in problems such as **procedural generation**, where
randomness tends to lead to more **natural**-seeming outputs. See the
`WaveFunctionCollapse` example for more details!

### ♻️ Running functions forwards _and_ backwards

With `satisfying` and `whenever`, we build a **predicate** on the input
parameters we supply. However, we can use propagators to create normal
functions, too! Consider the following function:

```haskell
celsius2fahrenheit :: MonadCell m => Prop m (Defined Double) -> Prop m (Defined Double)
celsius2fahrenheit c = c .* (9 ./ 5) .+ 32
```

This function converts a temperature written in **celsius** to **fahrenheit**.
The _interesting_ part of this, however, is that this is a function over
**propagator networks**. This means that, while we can use it as a _regular_
function...

```haskell
fahrenheit :: Maybe (Defined Double)
fahrenheit = forward celsius2fahrenheit 40.0 -- Just 104.0
```

... the "input" and "output" labels are meaningless! In fact, we can just as
easily pass a value to the function as the **output** and get back the
**input**!

```haskell
celsius :: Maybe (Defined Double)
celsius = backward celsius2fahrenheit 104.0 -- Just 40.0
```

> _Because neither `forward` nor `backward` require any parameter search, they
> are both computed by `Watson`, so the results are **pure**!_

<!--

```haskell
main :: IO ()
main = hspec do
  describe "Dinesman's Multiple Dwellings problem" do
    it "should be solved successfully" do
      dinesman >>= \result ->
        result `shouldBe` Just [ 3, 2, 4, 5, 1 ]

  describe "Sudoku" do
    it "should be solved successfully" do
      sudoku >>= \result ->
        result `shouldBe` Just solution

  describe "forward / backward" do
    it "works forwards"  do fahrenheit `shouldBe` Just 104.0
    it "works backwards" do celsius    `shouldBe` Just  40.0

solution :: [Intersect Value]
solution
  = [ 4, 5, 6,   1, 8, 3,   2, 7, 9
    , 8, 1, 2,   6, 9, 7,   5, 3, 4
    , 3, 7, 9,   5, 4, 2,   6, 1, 8

    , 1, 3, 4,   7, 6, 5,   9, 8, 2
    , 6, 9, 8,   2, 1, 4,   3, 5, 7
    , 7, 2, 5,   8, 3, 9,   4, 6, 1

    , 2, 6, 3,   9, 7, 8,   1, 4, 5
    , 5, 8, 1,   4, 2, 6,   7, 9, 3
    , 9, 4, 7,   3, 5, 1,   8, 2, 6
    ]
```

-->

## 🚂 Exploring the code

Now we've covered the **what**, maybe you're interested in the **how**! If
you're new to the **code** and want to get a feel for how the library works:

- The best place to start is probably in `Data/JoinSemilattice/Class/*`
  (we can ignore `Merge` until the next step). These will give you an idea of
  how we represent **relationships** (as opposed to **functions**) in `Holmes`.

- After that, `Control/Monad/Cell/Class.hs` gives an overview of the
  primitives for building a propagator network. In particular, see `unary` and
  `binary` for an idea of how we lift our **relationships** into a network.
  Here's where `src/Data/JoinSemilattice/Class/Merge` gets used, too, so the
  `write` primitive should give you an idea of why it's useful.

- `src/Data/Propagator.hs` introduces the high-level user-facing abstraction
  for stating constraints. Most of these functions are just wrapped calls to
  the aforementioned `unary` or `binary`, and really just add some syntactic
  sugar.

- Finally, `Control/Monad/MoriarT.hs` is a full implementation of the interface
  including support for **provenance** and **backtracking**. It also uses the
  functions in `Data/CDCL.hs` to optimise the parameter search. This is the
  base transformer on top of which we build `Control/Monad/Holmes.hs` _and_
  `Control/Monad/Watson.hs`.

Thus concludes our **whistle-stop tour** of my favourite sights in the
repository!

## ☎️ Questions?

If anything isn't clear, feel free to open an issue, or just message [me on
Twitter](https://twitter.com/am_i_tom); it's where you'll most likely get a
reply! I want this project to be an accessible way to approach the fields of
**propagators**, **constraint-solving**, and **CDCL**. If there's anything I
can do to improve this repository towards that goal, please **let me know**!

## 💐 Acknowledgements

- [Edward Kmett](https://twitter.com/kmett), whose
  [propagators repository](https://github.com/ekmett/propagators)\* gave us the
  `Prop` abstraction. I spent several months looking for alternative ways to
  represent computations, and never came close to something as neat.

- [Marco Sampellegrini](https://twitter.com/_alpacaaa), [Alex
  Peitsinis](https://twitter.com/alexpeits), [Irene
  Papakonstantinou](https://twitter.com/futumorphism), and plenty others who
  have helped me figure out how to present this library in a
  maximally-accessible way.

\* _This repository also approaches propagator network computations using Andy
Gill's [observable sharing](http://hackage.haskell.org/package/data-reify)
methods, which may be of interest! Neither `Holmes` nor `Watson` implement
this, as it requires some small breaks to purity and referential transparency,
of which users must be aware. We sacrifice some performance gains for ease of
use._
