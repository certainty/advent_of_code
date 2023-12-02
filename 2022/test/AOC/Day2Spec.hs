module AOC.Day2Spec where

import AOC.Day2 qualified as Day2
import Data.Text (Text, unlines)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty ()
import Test.Tasty.Hspec qualified ()
import Prelude hiding (unlines)

spec_day2 :: Spec
spec_day2 = describe "Day 2" $ do
  it "part1" $ do
    Day2.part1 fixture `shouldBe` Just 15
    
  it "part2" $ do
    Day2.part2 fixture `shouldBe` Just 12



fixture :: Text
fixture = unlines ["A Y", "B X", "C Z"]
