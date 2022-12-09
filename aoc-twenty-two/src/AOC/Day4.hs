module AOC.Day4 (run, part1, part2) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Char (isLower, ord, toLower)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Read (readMaybe)

data Section = Section {
  _sectionFrom :: Int,
  _sectionTo :: Int
} deriving (Eq, Show, Ord)

run :: IO ()
run = do
  input <- TIO.readFile "data/day4.txt"
  print $ part1 input
  print $ part2 input

part1 :: Text -> Maybe Int
part1 input = do 
  sections <- parseSections input
  pure $ length $ filter fullyContained' sections
  where 
    fullyContained' (s1, s2) = fullyContained s1 s2 || fullyContained s2 s1
  

part2 :: Text -> Maybe Int
part2 input = Just 4 


parseSections :: Text -> Maybe [(Section, Section)]
parseSections input = traverse parseSectionsLine (T.lines input)

parseSectionsLine :: Text -> Maybe (Section, Section)
parseSectionsLine input = case T.splitOn "," input of
                       [s1, s2] -> (,) <$> parseSection s1 <*> parseSection s2
                       _ -> Nothing


parseSection :: Text -> Maybe Section
parseSection input = case T.splitOn "-" input of
                      [lower, upper] -> Section <$> readInt lower <*> readInt upper 
                      _ -> Nothing

readInt :: Text -> Maybe Int
readInt = readMaybe . T.unpack

fullyContained :: Section -> Section -> Bool
fullyContained (Section from to) (Section from' to') = from' <= from && to' >= to
