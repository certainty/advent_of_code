module AOC.Day1Spec where

import AOC.Day1 qualified as Day1
import Data.Text (Text, unlines)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty ()
import Test.Tasty.Hspec qualified ()
import Prelude hiding (unlines)

spec_day1 :: Spec
spec_day1 = describe "Day 1" $ do
  it "implements part 1" $ do
    Day1.solve fixture `shouldBe` Right 24000

  it "implements part 2" $ do
    Day1.solvePartTwo fixture `shouldBe` Right 45000

fixture :: Text
fixture = unlines ["1000", "2000", "3000", "", "4000", "", "5000", "6000", "", "7000", "8000", "9000", "", "10000"]
