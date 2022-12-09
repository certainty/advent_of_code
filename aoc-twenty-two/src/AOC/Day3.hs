module AOC.Day3 (run, part1, part2) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Char (isLower, ord, toLower)
import Data.Set (Set)
import qualified Data.Set as Set

type Item = Char
type Batch = (Ruck, Ruck, Ruck)
type Ruck = Set Item


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

sharedItems :: [(Ruck, Ruck, Ruck)] -> [Item]
sharedItems ls = ls >>= sharedItem  
  where 
    sharedItem (a, b, c) = Set.toList $ a `Set.intersection` b `Set.intersection` c

parse :: Text -> Maybe [(Ruck, Ruck)]
parse input = traverse parseLine (T.lines input)

parsePart2 :: Text -> Maybe [Batch]
parsePart2 input = do
 batchify $ map textToRuck (T.lines input)
 where
    batchify :: [Ruck] -> Maybe [Batch]
    batchify [] = pure []
    batchify (a:b:c:xs) = do
      rest <- batchify xs
      pure $ (a, b, c) : rest
    batchify _ = Nothing


parseLine :: Text -> Maybe (Ruck, Ruck)
parseLine line 
  | T.length line < 2 = Nothing
  | otherwise = Just . toRucks $ T.splitAt mid line
 where
   mid = T.length line `div` 2
   toRucks (a, b) = (textToRuck a, textToRuck b)


textToRuck :: Text -> Ruck
textToRuck = Set.fromList . T.unpack 

findErrors :: [(Ruck, Ruck)] -> [Item]
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



