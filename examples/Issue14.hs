{-# Language BlockArguments #-}
{-# Language FlexibleContexts #-}
{-# Language PartialTypeSignatures #-}
{-# Language TypeApplications #-}
{-# Options_GHC -Wno-partial-type-signatures #-}
module Issue14 where

import Control.Monad.Watson (whenever, Watson)
import Data.Holmes hiding (whenever)
import Test.Hspec
import Data.Word (Word8)
import Data.Typeable (Typeable)

issue14 ::
  AbsR (jsl int) =>
  Enum (Raw (jsl int)) =>
  EqC jsl int =>
  EqR jsl =>
  Input (jsl int) =>
  Num (Raw (jsl int)) =>
  Num (jsl int) =>
  SumR (jsl int) =>
  Typeable int =>
  [ [jsl int] ]
issue14 = do
  let guesses = 2 `from` [1 .. 3] :: Config (Watson _) _
  guesses `whenever` \[a, b] ->
    (a .+ b) .== 5

spec_issue14 :: Spec
spec_issue14 = do
  it "handles constraints from the output when refining Defined inputs" do
    issue14 @Defined @Int `shouldBe` [ [2,3], [3,2] ]
  it "handles constraints from the output when refining Intersect inputs" do
    issue14 @Intersect @Word8 `shouldBe` [ [2,3], [3,2] ]
