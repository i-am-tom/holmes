{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Data.Holmes
Description : The public API for the @holmes@ library.
Copyright   : (c) Tom Harding, 2020
License     : MIT

This module includes almost everything you'd need to build a constraint-solving
computation. The module uses the 'Holmes' solver, but you may want to use the
functions in the "Control.Monad.Watson" module to avoid executing your code in
'IO'.
-}
module Data.Holmes
  ( Holmes
  , MonadCell

  , forward
  , backward
  , satisfying
  , shuffle
  , whenever

  , Config (..)
  , Input (..)
  , permute

  , AbsR (..)
  , BooleanR (..)
  , EqR (..), neR
  , FlatMapping (..)
  , FractionalR (..)
  , IntegralR (..)
  , Mapping (..)
  , OrdR (..), ltR, gtR, gteR
  , SumR (..), negateR, subR
  , Zipping (..)

  , Merge (..)
  , Result (..)

  , Defined (..)
  , Intersect (..)
  , using

  , Prop

  , (Prop..$), (Prop..>>=), Prop.zipWith'

  , (Prop..&&), Prop.all', Prop.allWithIndex', Prop.and'
  , (Prop..||), Prop.any', Prop.anyWithIndex', Prop.or'

  , Prop.not'
  , Prop.false, Prop.true

  , (Prop..*), (Prop../)
  , (Prop..+), (Prop..-)
  , (Prop..<), (Prop..<=), (Prop..>), (Prop..>=)
  , (Prop..==), (Prop../=), Prop.distinct
  , (Prop..%.), (Prop..*.), (Prop../.)

  , Prop.abs'
  , Prop.negate'
  , Prop.recip'
  ) where

import Control.Monad.Cell.Class (MonadCell)
import Control.Monad.Holmes (Holmes, satisfying, shuffle, whenever)
import Control.Monad.Watson (forward, backward)
import Data.Input.Config (Config (..), Input (..), permute)
import Data.JoinSemilattice.Class.Abs (AbsR (..))
import Data.JoinSemilattice.Class.Boolean (BooleanR (..))
import Data.JoinSemilattice.Class.Eq (EqR (..), neR)
import Data.JoinSemilattice.Class.FlatMapping (FlatMapping (..))
import Data.JoinSemilattice.Class.Fractional (FractionalR (..))
import Data.JoinSemilattice.Class.Integral (IntegralR (..))
import Data.JoinSemilattice.Class.Mapping (Mapping (..))
import Data.JoinSemilattice.Class.Merge (Merge (..), Result (..))
import Data.JoinSemilattice.Class.Ord (OrdR (..), ltR, gtR, gteR)
import Data.JoinSemilattice.Class.Sum (SumR (..), negateR, subR)
import Data.JoinSemilattice.Class.Zipping (Zipping (..))
import Data.JoinSemilattice.Defined (Defined (..))
import Data.JoinSemilattice.Intersect (Intersect (..), using)
import Data.Propagator (Prop)
import qualified Data.Propagator as Prop
