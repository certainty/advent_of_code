module AOC.Day3 (run, part1, part2) where

import Data.Char (isLower, ord, toLower)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

type Item = Char

type Rucksack = Set Item

type Batch = (Rucksack, Rucksack, Rucksack)

run :: IO ()
run = do
  input <- TIO.readFile "data/day3.txt"
  print $ part1 input
  print $ part2 input

part1 :: Text -> Maybe Int
part1 input = do
  compartments <- parse input
  let errors = findErrors compartments
  pure $ sum $ prioritizeItems errors

part2 :: Text -> Maybe Int
part2 input = do
  batches <- parsePart2 input
  pure $ sum $ prioritizeItems $ sharedItems batches

parse :: Text -> Maybe [(Rucksack, Rucksack)]
parse input = traverse parseLine (T.lines input)

parsePart2 :: Text -> Maybe [Batch]
parsePart2 input = do
  batchify $ map textToRucksack (T.lines input)
  where
    batchify :: [Rucksack] -> Maybe [Batch]
    batchify [] = pure []
    batchify (a : b : c : xs) = do
      rest <- batchify xs
      pure $ (a, b, c) : rest
    batchify _ = Nothing

parseLine :: Text -> Maybe (Rucksack, Rucksack)
parseLine line
  | T.length line < 2 = Nothing
  | otherwise = Just . toRucksacks $ T.splitAt mid line
  where
    mid = T.length line `div` 2
    toRucksacks (a, b) = (textToRucksack a, textToRucksack b)

sharedItems :: [(Rucksack, Rucksack, Rucksack)] -> [Item]
sharedItems ls = ls >>= sharedItem
  where
    sharedItem (a, b, c) = Set.toList $ a `Set.intersection` b `Set.intersection` c

textToRucksack :: Text -> Rucksack
textToRucksack = Set.fromList . T.unpack

findErrors :: [(Rucksack, Rucksack)] -> [Item]
findErrors compartments = compartments >>= sharedItems'
  where
    sharedItems' (left, right) = Set.toList $ left `Set.intersection` right

prioritizeItems :: [Item] -> [Int]
prioritizeItems = fmap itemToPrio

itemToPrio :: Item -> Int
itemToPrio c
  | isLower c = lowerPrio c
  | otherwise = lowerPrio (toLower c) + 26
  where
    lowerPrio c' = ord c' - 96
