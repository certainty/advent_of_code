module AOC.Day3Spec where

import AOC.Day3 qualified as Day3
import Data.Text (Text, unlines)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty ()
import Test.Tasty.Hspec qualified ()
import Prelude hiding (unlines)

spec_day3 :: Spec
spec_day3 = describe "Day 3" $ do
  it "part1" $ do
    Day3.part1 fixture `shouldBe` Just 157 
  it "part2" $ do
    Day3.part2 fixture `shouldBe` Just 70 

    
fixture :: Text
fixture = unlines 
   ["vJrwpWtwJgWrhcsFMMfFFhFp", 
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL", 
    "PmmdzqPrVvPwwTWBwg", 
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
    "ttgJtRGJQctTZtZT",
    "CrZsJsPPZsGzwwsLwLmpwMDw"]
