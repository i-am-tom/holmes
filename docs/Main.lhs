# 🔎 Holmes

?> It is an old maxim of mine that when you have excluded the impossible,
whatever remains, however improbable, must be the **truth**.

<!--
```haskell
{-# LANGUAGE BlockArguments #-}
module Main where

import qualified Futoshiki
import qualified Parameters
import qualified Practices
import qualified Predicates
import qualified Programs
import qualified WaveFunctionCollapse
import Test.Hspec
```
-->

`Holmes` is a library and reference implementation for solving constraint
problems using **propagator networks** and **conflict-directed clause
learning**.  Describe the **parameters**, explain the **constraints**, and
`Holmes` will do the rest.

## 🐢 Quickstart

It's our first case, so let's solve a classic: [Dinesman's multiple dwellings
problem](https://rosettacode.org/wiki/Dinesman%27s_multiple-dwelling_problem)!

> Baker, Cooper, Fletcher, Miller, and Smith live on different floors of an
apartment house that contains only five floors.
> 
> - Baker does not live on the top floor.
> - Cooper does not live on the bottom floor.
> - Fletcher does not live on either the top or the bottom floor.
> - Miller lives on a higher floor than does Cooper.
> - Smith does not live on a floor adjacent to Fletcher's.
> - Fletcher does not live on a floor adjacent to Cooper's.
> 
> Where does everyone live?

When we translate this into a format that `Holmes` can understand, we tend to
break the problem into two distinct parts: the __parameters__ and the
__constraints__.

```haskell
import Data.Holmes

dinesman :: IO (Maybe [ Defined Int ])
dinesman = do
  -- Parameters: what is our search space?
  let guesses = 5 `from` [1 .. 5]

  -- Constraints: what does a solution look like?
  guesses `satisfying` \[ baker, cooper, fletcher, miller, smith ] -> and'
    [ distinct [ baker, cooper, fletcher, miller, smith ]
    , baker ./= 5
    , cooper ./= 1
    , fletcher ./= 1 .&& fletcher ./= 5
    , miller .> cooper
    , abs (smith .- fletcher) ./= 1
    , abs (fletcher .- cooper) ./= 1
    ]
```

In this case, we have five parameters: the **floor numbers** of the five
occupants. These must be between `1` and `5`, so we use the library's `from`
function to generate our `guesses`.

After that, we simply ask `Holmes` to produce the list of **configurations**
that match our given constraints. In this case, the `satisfying` function takes
an input generator and a boolean predicate, then returns the permissible input
configurations.

## 📚 Reference

For a set of **tutorials** to understand the fundamentals:

- 🛠 [Constructing parameters](/Parameters)
- 🔌 [Wiring predicates](/Predicates)
- 🔮 [Running programs](/Programs)
<!-- - 🔬 [Advanced practices](/Practices) -->

Otherwise, if you'd just like to see some **example projects**:

- 🔢 [Futoshiki](/Futoshiki)
- 🌊 [WaveFunctionCollapse](/WaveFunctionCollapse)

_If something isn't obvious, please feel free to [open an
issue](https://github.com/i-am-tom/holmes/issues)! I'll do my best either to
answer the question or write a new reference guide!_

# 💐 Acknowledgements

I first came to propagators through [Edward Kmett's](https://twitter.com/kmett)
[`propagators` repository](https://github.com/ekmett/propagators), then went on
to implement the original papers: [The Art of the
Propagator](https://dspace.mit.edu/handle/1721.1/44215) and [Propagation
networks: a flexible and expressive substrate for
computation](https://dspace.mit.edu/handle/1721.1/54635).

Having then tried to add backtracking* and "haskell-ify" the paper's proposed
interface, it was funny (yet wholly unsurprising) to see myself end up back
where Edward had started. While there are far more direct approaches (such as
skipping the `Prop` abstraction entirely, favouring a direct `Cell` API), this
predicate- and computation-constructing API proved to be the most intuitive to
the various people who've offered me help throughout. It therefore only seems
fair that I doubly thank Edward, whose formulation of the `Prop` abstraction is
central to the final API of this library.

To [Marco Sampellegrini](https://twitter.com/_alpacaaa),
[Alex Peitsinis](https://twitter.com/alexpeits), [Irene
Papakonstantinou](https://twitter.com/futumorphism), and plenty others: thank
you for listening to me ramble on about propagators for about eight months, and
providing invaluable feedback about how best to explain the concepts that
underpin this library.

\* _In favour of the "TMS" approach explained in the latter paper._

<!--
```haskell
main :: IO ()
main = hspec do
  describe "Multiple Dwellings" do
    it "should calculate the answer!" do
      result <- dinesman
      
      result `shouldBe` Just [ 3, 2, 4, 5, 1 ]

  Futoshiki.main
  Parameters.main
  Predicates.main
  Programs.main
  Practices.main
  WaveFunctionCollapse.main
```
-->
