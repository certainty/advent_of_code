module AOC.Day1 (run, part1, part2) where

import Data.List (sortOn)
import Data.List.Split qualified as S
import Data.Ord
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Text.Read (readMaybe)

run :: IO ()
run = do
  input <- TIO.readFile "data/day1.txt"
  print $ part1 input
  print $ part2 input

part1 :: Text -> Maybe Int
part1 input = maximum <$> caloriesPerElve input

part2 :: Text -> Maybe Int
part2 input = sum . take 3 . desc <$> caloriesPerElve input
  where
    desc = sortOn Down

-- Read encoded calories and reaturn the total calories per elve
caloriesPerElve :: Text -> Maybe [Int]
caloriesPerElve input = do
  let chunks = S.splitWhen (== "") (T.lines input)
  fmap sum <$> traverse readChunk chunks
  where
    readChunk :: [Text] -> Maybe [Int]
    readChunk = traverse (readMaybe . T.unpack)
