module AOC.Day3 (run, part1, part2) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Char (isLower, ord, toLower)
import Data.Set (Set)
import qualified Data.Set as Set

run :: IO ()
run = do
  input <- TIO.readFile "data/day3.txt"
  print $ part1 input
  print $ part2 input

part1 :: Text -> Maybe Int
part1 input = do 
  compartments <- parse input 
  let errors = findErrors compartments
  pure $ sum $ prioritizeErrors errors

part2 :: Text -> Maybe Int
part2 input = Just 3

type Item = Char
type Compartment = Set Item

parse :: Text -> Maybe [(Compartment, Compartment)]
parse input = traverse parseLine (T.lines input)

parseLine :: Text -> Maybe (Compartment, Compartment)
parseLine line 
  | T.length line < 2 = Nothing
  | otherwise = Just . toCompartments $ T.splitAt mid line
 where
   mid = T.length line `div` 2
   toCompartments (a, b) = (textToCompartment a, textToCompartment b)

textToCompartment :: Text -> Compartment
textToCompartment = Set.fromList . T.unpack 

findErrors :: [(Compartment, Compartment)] -> [Item]
findErrors compartments = compartments >>= sharedItems
  where
    sharedItems (left, right) = Set.toList $ left `Set.intersection` right

prioritizeErrors :: [Item] -> [Int]
prioritizeErrors = fmap itemToPrio

itemToPrio :: Item -> Int
itemToPrio c 
  | isLower c = lowerPrio c
  | otherwise = lowerPrio (toLower c) + 26
 where 
  lowerPrio c' = ord c' - 96



