module AOC.Day1Spec where

import qualified AOC.Day1 as Day1
import Data.Text (Text, unlines)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty ()
import qualified Test.Tasty.Hspec ()
import Prelude hiding (unlines)

spec_day1 :: Spec
spec_day1 = describe "Day 1" $ do
  it "works" $ do
    Day1.solve fixture `shouldBe` (Right 24000)

fixture :: Text
fixture = unlines $ ["1000", "2000", "3000", "", "4000", "", "5000", "6000", "", "7000", "8000", "9000", "", "1000"]
