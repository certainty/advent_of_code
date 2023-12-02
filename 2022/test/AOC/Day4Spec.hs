module AOC.Day4Spec where

import AOC.Day4 qualified as Day4
import Data.Text (Text, unlines)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty ()
import Test.Tasty.Hspec qualified ()
import Prelude hiding (unlines)

spec_day4 :: Spec
spec_day4 = describe "Day 4" $ do
  it "part1" $ do
    Day4.part1 fixture `shouldBe` Just 2 
  it "part2" $ do
    Day4.part2 fixture `shouldBe` Just 4 
    
fixture :: Text
fixture = unlines  
  [
     "2-4,6-8",
     "2-3,4-5",
     "5-7,7-9",
     "2-8,3-7",
     "6-6,4-6",
     "2-6,4-8"
  ]
