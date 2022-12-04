module AOC.Day1 (run, solve, solvePartTwo, readCalories) where

import Data.List (sortOn)
import qualified Data.List.Split as S
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Read

run :: IO ()
run = do
  input <- TIO.readFile "data/day1.txt"
  case solve input of
    Left err -> print err
    Right n -> TIO.putStr "Part1: " >> print n

  case solvePartTwo input of
    Left err -> print err
    Right n -> TIO.putStr "Part2: " >> print n

solve :: Text -> Either String Int
solve input = maximum <$> readCalories input

solvePartTwo :: Text -> Either String Int
solvePartTwo input = do
  calories <- readCalories input
  pure $ sum <$> take 3 $ (sortOn Down) calories

-- Read encoded calories and reaturn the total calories per elve
readCalories :: Text -> Either String [Int]
readCalories input = do
  let caloriesPerElve = S.splitWhen (== "") (T.lines input)
  caloriesNumericPerElve <- traverse (traverse (\t -> fst <$> decimal t)) caloriesPerElve
  pure $ sum <$> caloriesNumericPerElve
