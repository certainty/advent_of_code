module AOC.Day2 (part1) where

import Data.Text (Text)
import Data.Text qualified as T

data Hand = Rock | Paper | Scissors deriving (Ord, Enum, Eq, Show)

part1 :: Text -> Maybe Int
part1 input = do
  rounds <- parse input
  pure (sum $ scoreRound <$> rounds)

parse :: Text -> Maybe [(Hand, Hand)]
parse input = traverse parseLine (T.lines $ input)

-- A line is two characters
-- The left character is player A
-- The right character is player B
-- Each character encodes a hand
-- A, X => Rock
-- B, Y => Paper
-- C, Z => Scissors
parseLine :: Text -> Maybe (Hand, Hand)
parseLine line = do
  case T.words line of
    [left, right] -> do
      leftHand <- parseHand left
      rightHand <- parseHand right
      pure (leftHand, rightHand)
    _ -> Nothing
  where
    parseHand :: Text -> Maybe Hand
    parseHand = \case
      "A" -> Just Rock
      "X" -> Just Rock
      "B" -> Just Paper
      "Y" -> Just Paper
      "C" -> Just Scissors
      "Z" -> Just Scissors
      _ -> Nothing

scoreRound :: (Hand, Hand) -> Int
scoreRound hands@(_, me) = scoreOutcome hands + scoreHand me

scoreHand :: Hand -> Int
scoreHand Rock = 1
scoreHand Paper = 2
scoreHand Scissors = 3

scoreOutcome :: (Hand, Hand) -> Int
scoreOutcome (Scissors, Rock) = 6
scoreOutcome (Rock, Paper) = 6
scoreOutcome (Paper, Scissors) = 6
scoreOutcome (other, me)
  | other == me = 3
  | otherwise = 0
