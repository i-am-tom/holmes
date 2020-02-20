{-# LANGUAGE BlockArguments #-}
module Test.Data.JoinSemilattice.Class.Abs where

import Data.Holmes (AbsR (..))
import Hedgehog

absR_absR :: (AbsR x, Eq x, Num x, Show x) => Gen x -> Property
absR_absR gen = property do
  a <- forAll gen

  let ( _, b ) = absR ( a, mempty )
  annotateShow b
  b === abs a

  let ( a', _ ) = absR ( mempty, b )
  annotateShow a'
  a' <> a === a
