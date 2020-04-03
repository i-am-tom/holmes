{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : Data.Propagator
Description : The high-level propagator abstraction.
Copyright   : (c) Tom Harding, 2020
License     : MIT

The real heart of a propagator network is the cell-level interaction, but it
doesn't come with a particularly pleasant API. The solution is the 'Prop'
abstraction, which hides away some of the more gruesome internals.

This module exposes a set of functions to construct propagator networks with a
"focal point", which we can intuit as being the "output" of the functions we're
used to writing.

The important thing to note is that most of these functions allow for
__multi-directional__ information flow. While '(.&&)' might /look/ like '(&&)',
it allows the inputs to be computed from the outputs, so it's a lot more
capable. Think of these functions as a way to build equations that we can
re-arrange as need be.
-}
module Data.Propagator
  ( Prop, up, down, lift, over, lift2, unary, binary

  , (.&&), all', allWithIndex', and'
  , (.||), any', anyWithIndex', or'
  , false, not', true, exactly, choose

  , (.==), (./=), distinct

  , (.>), (.>=), (.<), (.<=)

  , (.+), (.-), negate'
  , (.*.), (./.), (.%.)
  , (.*), (./), recip'
  , abs'

  , (.$)
  , zipWith'
  , (.>>=)
  ) where

import Control.Monad.Cell.Class (MonadCell (..))
import qualified Control.Monad.Cell.Class as Cell
import Data.JoinSemilattice.Class.Abs (AbsR (..))
import Data.JoinSemilattice.Class.Boolean (BooleanR (..))
import Data.JoinSemilattice.Class.Eq (EqR (..), neR)
import Data.JoinSemilattice.Class.FlatMapping (FlatMapping (..))
import Data.JoinSemilattice.Class.Fractional (FractionalR (..), divideR, multiplyR, recipR)
import Data.JoinSemilattice.Class.Integral (IntegralR (..), divR, modR, timesR)
import Data.JoinSemilattice.Class.Lifting (Lifting (..))
import Data.JoinSemilattice.Class.Mapping (Mapping (..))
import Data.JoinSemilattice.Class.Merge (Merge)
import Data.JoinSemilattice.Class.Ord (OrdR (..), gtR, gteR, ltR)
import Data.JoinSemilattice.Class.Sum (SumR (..), negateR, subR)
import Data.JoinSemilattice.Class.Zipping (Zipping (..))
import Data.Kind (Type)

-- | A propagator network with a "focus" on a particular cell. The focus is the
-- cell that typically holds the result we're trying to compute.
data Prop (m :: Type -> Type) (content :: Type) where

  Nullary
    :: m (Cell m x)
    -> Prop m x

  Unary
    :: Merge x
    => (forall f. MonadCell f => Cell f x -> Cell f y -> f ())
    -> Prop m x
    -> Prop m y

  Binary
    :: ( Merge x
       , Merge y
       )
    => (forall f. MonadCell f => Cell f x -> Cell f y -> Cell f z -> f ())
    -> Prop m x
    -> Prop m y
    -> Prop m z

instance (AbsR x, SumR x, Num x, MonadCell m)
    => Num (Prop m x) where
  (+) = Binary (Cell.binary addR)
  (-) = Binary (Cell.binary subR)

  abs    = Unary (Cell.unary absR)
  negate = Unary (Cell.unary negateR)

  (*) = Binary \these those total ->
    -- Division isn't in 'Num', so we can't invert!
    Cell.watch these \this -> Cell.with those \that ->
      Cell.write total (this * that)

  fromInteger = Nullary . Cell.fill . Prelude.fromInteger
  signum = Unary \these those -> Cell.watch these (Cell.write those . signum)

instance (AbsR x, Fractional x, FractionalR x, Num x, MonadCell m)
    => Fractional (Prop m x) where
  (/) = Binary (Cell.binary divideR)

  fromRational = Nullary . Cell.fill . Prelude.fromRational
  recip = Unary (Cell.unary recipR)

-- | Lift a cell into a propagator network. Mostly for internal library use.
up :: Applicative m => Cell m x -> Prop m x
up = Nullary . pure

-- | Lower a propagator network's focal point down to a cell. Mostly for
-- internal library use.
down :: (MonadCell m, Monoid x) => Prop m x -> m (Cell m x)
down = \case
  Nullary x -> x

  Unary f a -> do
    x <- down a
    y <- Cell.make
    
    f x y
    pure y

  Binary f a b -> do
    x <- down a
    y <- down b
    z <- Cell.make

    f x y z
    pure z

lift :: forall f m c x. (MonadCell m, c x) => Lifting f c => x -> Prop m (f x)
lift = Nullary . fill . lift'

-- | Lift a regular function into a propagator network. The function is lifted
-- into a relationship with one-way information flow.
over :: (Merge x, Merge y) => (x -> y) -> Prop m x -> Prop m y
over f = Unary \x y -> Cell.watch x (Cell.write y . f)

-- | Lift a unary relationship into a propagator network. Unlike 'over', this
-- allows information to travel in both directions.
unary :: (Merge x, Merge y) => ((x, y) -> (x, y)) -> Prop m x -> Prop m y
unary f = Unary (Cell.unary f)

-- | Lift a binary relationship into a propagator network. This allows
-- three-way information flow.
binary :: (Merge x, Merge y, Merge z) => ((x, y, z) -> (x, y, z)) -> Prop m x -> Prop m y -> Prop m z
binary f = Binary (Cell.binary f)

-- | Lift a regular binary function into a propagator network. The function is
-- lifted into a relationship between three variables where information only
-- flows in one direction.
lift2 :: (Merge x, Merge y, Merge z) => (x -> y -> z) -> Prop m x -> Prop m y -> Prop m z
lift2 f = binary \(x, y, _) -> (mempty, mempty, f x y)

-- | Different parameter types come with different representations for 'Bool'.
-- This function takes two propagator networks focusing on boolean values, and
-- produces a new network in which the focus is the conjunction of the two
-- values.
--
-- It's a lot of words, but the intuition is, "'(&&)' over propagators".
(.&&) :: BooleanR f => Prop m (f Bool) -> Prop m (f Bool) -> Prop m (f Bool)
(.&&) = Binary (Cell.binary andR)

infixr 3 .&&

-- | Run a predicate on all values in a list, producing a list of propagator
-- networks focusing on boolean values. Then, produce a new network with a
-- focus on the conjunction of all these values.
--
-- In other words, "'all' over propagators".
all' :: (BooleanR f, MonadCell m) => (x -> Prop m (f Bool)) -> [ x ] -> Prop m (f Bool)
all' f = and' . map f

-- | The same as the 'all'' function, but with access to the index of the
-- element within the array. Typically, this is useful when trying to relate
-- each element to /other/ elements within the array.
--
-- /For example, cells "surrounding" the current cell in a conceptual "board"./
allWithIndex' :: (BooleanR f, MonadCell m) => (Int -> x -> Prop m (f Bool)) -> [ x ] -> Prop m (f Bool)
allWithIndex' f = all' (uncurry f) . zip [0 ..]

-- | Given a list of propagator networks with a focus on boolean values, create
-- a new network with a focus on the conjugation of all these values.
--
-- In other words, "'and' over propagators".
and' :: (BooleanR f, MonadCell m) => [ Prop m (f Bool) ] -> Prop m (f Bool)
and' = foldr (.&&) true

-- | Run a predicate on all values in a list, producing a list of propagator
-- networks focusing on boolean values. Then, produce a new network with a
-- focus on the disjunction of all these values.
--
-- In other words, "'any' over propagators".
any' :: (BooleanR f, MonadCell m) => (x -> Prop m (f Bool)) -> [ x ] -> Prop m (f Bool)
any' f = or' . map f

-- | The same as the 'any'' function, but with access to the index of the
-- element within the array. Typically, this is useful when trying to relate
-- each element to /other/ elements within the array.
--
-- /For example, cells "surrounding" the current cell in a conceptual "board"./
anyWithIndex' :: (BooleanR f, MonadCell m) => (Int -> x -> Prop m (f Bool)) -> [ x ] -> Prop m (f Bool)
anyWithIndex' f = any' (uncurry f) . zip [0 ..]

-- | Asserts that exactly n of the elements must match the given predicate.
exactly :: (BooleanR f, MonadCell m) => Int -> (x -> Prop m (f Bool)) -> [x] -> Prop m (f Bool)
exactly n f xs = 
    let l = length xs
        choices = choose l n
        applyChoice picks = zipWith (\pick x -> if pick then f x else not' (f x)) picks xs
    in or' (map (and'.applyChoice) choices)

-- | Utility function that calculates all possible ways to pick k values out of n.
-- It returns a list of picks, where each pick contains a boolean indicating whether
-- that value was picked
choose :: Int -> Int -> [[Bool]]
choose n k = 
  if k<0 || k>n
    then []
    else
      if n==0
        then [[]]
        else
          map (False:) (choose (pred n) k) ++
          map (True:)  (choose (pred n) (pred k))

-- | Different parameter types come with different representations for 'Bool'.
-- This value is a propagator network with a focus on a polymorphic "falsey"
-- value.
false :: (BooleanR f, MonadCell m) => Prop m (f Bool)
false = Nullary (Cell.fill falseR)

-- | Given a propagator network with a focus on a boolean value, produce a
-- network with a focus on its negation.
--
-- ... It's "'not' over propagators".
not' :: (BooleanR f, MonadCell m) => Prop m (f Bool) -> Prop m (f Bool) 
not' = Unary (Cell.unary notR)

-- | Given a list of propagator networks with a focus on boolean values, create
-- a new network with a focus on the disjunction of all these values.
--
-- In other words, "'or' over propagators".
or' :: (BooleanR f, MonadCell m) => [ Prop m (f Bool) ] -> Prop m (f Bool)
or' = foldr (.||) false

-- | Different parameter types come with different representations for 'Bool'.
-- This value is a propagator network with a focus on a polymorphic "truthy"
-- value.
true :: (BooleanR f, MonadCell m) => Prop m (f Bool)
true = Nullary (Cell.fill trueR)

-- | Calculate the disjunction of two boolean propagator network values.
(.||) :: BooleanR f => Prop m (f Bool) -> Prop m (f Bool) -> Prop m (f Bool)
(.||) = Binary (Cell.binary orR)

infixr 2 .||

-- | Given two propagator networks, produce a new propagator network with the
-- result of testing the two for equality.
--
-- In other words, "it's '(==)' for propagators".
(.==) :: (EqR f, EqC f x, MonadCell m) => Prop m (f x) -> Prop m (f x) -> Prop m (f Bool)
(.==) = Binary (Cell.binary eqR)

infix 4 .==

-- | Given two propagator networks, produce a new propagator network with the
-- result of testing the two for inequality.
--
-- In other words, "it's '(/=)' for propagators".
(./=) :: (EqR f, EqC f x, MonadCell m) => Prop m (f x) -> Prop m (f x) -> Prop m (f Bool)
(./=) = Binary (Cell.binary neR)

infix 4 ./=

-- | Given a list of networks, produce the conjunction of '(./=)' applied to
-- every possible pair. The resulting network's focus is the answer to whether
-- every propagator network's focus is different to the others.
--
-- /Are all the values in this list distinct?/
distinct :: (EqR f, EqC f x, MonadCell m) => [ Prop m (f x) ] -> Prop m (f Bool)
distinct = \case
  x : xs -> all' (./= x) xs .&& distinct xs
  [    ] -> Nullary (Cell.fill trueR)

-- | Given two propagator networks, produce a new network that calculates
-- whether the first network's focus be greater than the second.
--
-- In other words, "it's '(>)' for propagators".
(.>) :: (OrdR f, OrdC f x, MonadCell m) => Prop m (f x) -> Prop m (f x) -> Prop m (f Bool)
(.>) = Binary (Cell.binary gtR)

infix 4 .>

-- | Given two propagator networks, produce a new network that calculates
-- whether the first network's focus be greater than or equal to the second.
--
-- In other words, "it's '(>=)' for propagators".
(.>=) :: (OrdR f, OrdC f x, MonadCell m) => Prop m (f x) -> Prop m (f x) -> Prop m (f Bool)
(.>=) = Binary (Cell.binary gteR)

infix 4 .>=

-- | Given two propagator networks, produce a new network that calculates
-- whether the first network's focus be less than the second.
--
-- In other words, "it's '(<)' for propagators".
(.<) :: (OrdR f, OrdC f x, MonadCell m) => Prop m (f x) -> Prop m (f x) -> Prop m (f Bool)
(.<) = Binary (Cell.binary ltR)

infix 4 .<

-- | Given two propagator networks, produce a new network that calculates
-- whether the first network's focus be less than or equal to the second.
--
-- In other words, "it's '(<=)' for propagators".
(.<=) :: (OrdR f, OrdC f x, MonadCell m) => Prop m (f x) -> Prop m (f x) -> Prop m (f Bool)
(.<=) = Binary (Cell.binary lteR)

infix 4 .<=

-- | Given two propagator networks, produce a new network that focuses on the
-- sum of the two given networks' foci.
--
-- /... It's '(+)' lifted over propagator networks./
(.+) :: (SumR x, MonadCell m) => Prop m x -> Prop m x -> Prop m x
(.+) = Binary (Cell.binary addR)

infixl 6 .+

-- | Produce a network that focuses on the /negation/ of another network's
-- focus.
--
-- /... It's 'negate' lifted over propagator networks./
negate' :: (Num x, SumR x, MonadCell m) => Prop m x -> Prop m x
negate' = Unary (Cell.unary negateR)

-- | Given two propagator networks, produce a new network that focuses on the
-- difference between the two given networks' foci.
--
-- /... It's '(-)' lifted over propagator networks./
(.-) :: (SumR x, MonadCell m) => Prop m x -> Prop m x -> Prop m x
(.-) = Binary (Cell.binary subR)

infixl 6 .-

-- | Given two propagator networks, produce a new network that focuses on the
-- product between the two given networks' /integral/ foci.
--
-- /... It's '(*)' lifted over propagator networks./ Crucially, the reverse
-- information flow uses __integral division__, which should work the same way
-- as 'div'.
(.*.) :: (Num x, IntegralR x) => Prop m x -> Prop m x -> Prop m x
(.*.) = Binary (Cell.binary timesR)

infixl 7 .*.

-- | Given two propagator networks, produce a new network that focuses on the
-- division of the two given networks' /integral/ foci.
--
-- /... It's 'div' lifted over propagator networks./
(./.) :: (IntegralR x, MonadCell m) => Prop m x -> Prop m x -> Prop m x
(./.) = Binary (Cell.binary divR)

infixl 7 ./.

-- | Given two propagator networks, produce a new network that focuses on the
-- modulo of the two given networks' /integral/ foci.
--
-- /... It's 'mod' lifted over propagator networks./
(.%.) :: (IntegralR x, MonadCell m) => Prop m x -> Prop m x -> Prop m x
(.%.) = Binary (Cell.binary modR)

infixl 7 .%.

-- | Given two propagator networks, produce a new network that focuses on the
-- product of the two given networks' foci.
--
-- /... It's '(*)' lifted over propagator networks./ The reverse information
-- flow is fractional division, '(/)'.
(.*) :: (FractionalR x, MonadCell m) => Prop m x -> Prop m x -> Prop m x
(.*) = Binary (Cell.binary multiplyR)

infixl 7 .*

-- | Given two propagator networks, produce a new network that focuses on the
-- division of the two given networks' foci.
--
-- ... It's '(/)' lifted over propagator networks.
(./) :: (FractionalR x, MonadCell m) => Prop m x -> Prop m x -> Prop m x
(./) = Binary (Cell.binary divideR)

infixl 7 ./

-- | Produce a network that focuses on the /reciprocal/ of another network's
-- focus.
--
-- /... It's 'recip' lifted over propagator networks./
recip' :: (Num x, FractionalR x, MonadCell m) => Prop m x -> Prop m x
recip' = Unary (Cell.unary recipR)

-- | Produce a network that focuses on the /absolute value/ of another
-- network's focus.
--
-- /... It's 'abs' lifted over propagator networks./
abs' :: (AbsR x, MonadCell m) => Prop m x -> Prop m x
abs' = Unary (Cell.unary absR)

-- | Lift a regular function over a propagator network /and/ its parameter
-- type. Unlike 'over', this function abstracts away the specific behaviour of
-- the parameter type (such as 'Data.JoinSemilattice.Defined.Defined').
(.$) :: (Mapping f c, c x, c y) => (x -> y) -> Prop m (f x) -> Prop m (f y)
(.$) f = Unary (Cell.unary (mapR (Just f, Nothing)))

-- | Lift a three-way relationship over two propagator networks' foci to
-- produce a third propagator network with a focus on the third value in the
-- relationship.
--
-- /... It's 'Control.Applicative.liftA2' for propagators./
zipWith' :: (Zipping f c, c x, c y, c z) => (x -> y -> z) -> Prop m (f x) -> Prop m (f y) -> Prop m (f z)
zipWith' f = Binary (Cell.binary (zipWithR (Just (uncurry f), Nothing, Nothing)))

-- | Produce a network in which the raw values of a given network are used to
-- produce new parameter types. See the "wave function collapse" demo for an
-- example usage.
(.>>=) :: (FlatMapping f c, c x, c y) => Prop m (f x) -> (x -> f y) -> Prop m (f y)
(.>>=) xs f = Unary (Cell.unary (flatMapR (Just f, Nothing))) xs
