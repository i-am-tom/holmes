# 🔮 Running programs

?> You know my **methods**. Apply them.

<!--
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Programs where

import Data.Holmes
import Test.Hspec

main :: Spec
main = do
  describe "backward" do
    it "reverses (+ 5)" sub5_example

  describe "satisfying" do
    it "finds triples" valid_triples
```
-->

Now we know how to define **parameters** and **constraints**, there's very
little left to see!

## ♻️  A multi-directional refresher

When we discussed [predicates](/Predicates), we saw two of the functions for
running computations _without_ input parameters: `forward` and `backward`. This
allows us to take advantage of the multi-directional information flow in
`Holmes` for simple computations:

```haskell
sub5 :: Defined Int -> IO (Maybe (Defined Int))
sub5 = backward (.+ 5)

sub5_example :: Expectation
sub5_example = sub5 8 >>= \x -> x `shouldBe` Just 3
```

Here, we've run the `+ 5` function _backwards_ to produce a `- 5` function. We
could just as easily run it `forward` to get the `+ 5` function - the choice is
ours!

What's interesting about `forward` and `backward` is there's no **search**
element. This is really the most direct way to **translate** our programs into
the language of propagators.

## 🌯 Satisfying our needs

The real strength of `Holmes` is found in problems for which we need to
**search** a problem space and potentially **backtrack** over several candidate
solutions. When this happens, we combine what we know about
[parameters](/Parameters) with what we know about [predicates](/Predicates)
using the `satisfying` function:

```haskell
inputs :: Applicative m => Config m (Defined Int)
inputs = 3 `from` [1 .. 10]

valid_triples :: IO ()
valid_triples = do
  results <- inputs `whenever` \[ x, y, z ] ->
    (x .*. x) .+ (y .*. y) .== (z .*. z) .&& x .< y

  results `shouldBe` [ [ 3, 4, 5 ], [ 6, 8, 10 ] ]
```

Here, we've defined an input search space, along with a predicate on the inputs
that must be satisfied, and then extracted the results from the computation.
**Success**!

## 🎓 Graduation

You now know **everything** you need in order to build constraint-solving
programs. **Hooray**!

At this point, we've covered the entire **high-level** propagator API, and that
should cover the main use cases. At this point, it's probably worth looking at
[an example](/Futoshiki). just to make sure the concepts make sense together.
If you're feeling _very_ comfortable, take a look at the [advanced
usages](/Practices) to see how you can extend the library to your needs.
