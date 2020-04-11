{-# OPTIONS_GHC -Wno-missing-methods #-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Test.Regression.Issue7 where

import Data.Function ((&))
import Data.Hashable (Hashable)
import Data.Holmes
import GHC.Generics (Generic)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe)

-------------------------------------------------------------------------------
-- #7: stack overflow / infinite loop when solving with Intersect instead of
-- Defined

countEqual
  :: ( Eq (f x)
     , Lifting f c
     , Mapping f c
     , c x, c v
     , Num v, Num (f v), SumR (f v)
     , MonadCell m
     )
  => [(Int, x)]
  -> [Prop m (f x)]
  -> Prop m (f v)
countEqual values cells = foldr (.+) (lift 0) (map f values)
  where
    f (index, expected) = cells !! index & over \actual ->
      if lift' expected == actual then 1 else 0

spec_17_defined :: Spec
spec_17_defined = describe "Issue #17" do
  it "Defined" do
    let checks :: [( Int, Int )]
        checks = [( 0, 3 )]

        threshold :: MonadCell m => Prop m (Defined Int)
        threshold = lift 0

    example <- (1 `from` [ 1 ]) `satisfying` \cells -> do
      countEqual checks cells .>= threshold

    example `shouldBe` Just [ 1 ]

  it "Intersect" do
    let checks :: [( Int, Val4 )]
        checks = [( 0, 3 )]

        threshold :: MonadCell m => Prop m (Intersect Val4)
        threshold = lift 0

    example <- (1 `from` [ 1 ]) `satisfying` \cells -> do
      countEqual checks cells .>= threshold

    example `shouldBe` Just [ 1 ]

newtype Val4 = V4 Int
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)

instance Num Val4 where

  fromInteger = toEnum . fromInteger

  -- this is not a valid Num instance, we just want to use
  -- it for counting
  V4 a + V4 b = if a + b > 4 then V4 4 else V4 (a + b)
  V4 a - V4 b = if a - b < 0 then V4 0 else V4 (a - b)

instance Enum Val4 where
  toEnum n
    | n < 0 || n > 4 = error $ "toEnum Val4 out of bounds: " ++ show n
    | otherwise = V4 n

  fromEnum v@(V4 m)
    | m < 0 || m > 4 = error $ "fromEnum Val4 out of bounds: " ++ show v
    | otherwise = m

instance Bounded Val4 where
  minBound = toEnum 0
  maxBound = toEnum 4
