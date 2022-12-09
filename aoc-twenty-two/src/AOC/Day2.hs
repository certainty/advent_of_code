module AOC.Day2 (run, part1, part2) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

data Hand = Rock | Paper | Scissors deriving (Ord, Enum, Eq, Show)

data Result = Win | Lose | Draw deriving (Eq, Show)

type LineDecoder = ((Text, Text) -> Maybe (Hand, Hand))

run :: IO ()
run = do
  input <- TIO.readFile "data/day2.txt"
  print $ part1 input
  print $ part2 input

part1 :: Text -> Maybe Int
part1 = evaluateInput lineDecoderPart1

part2 :: Text -> Maybe Int
part2 = evaluateInput lineDecoderPart2

evaluateInput :: LineDecoder -> Text -> Maybe Int
evaluateInput decoder input = do
  rounds <- decodeInput decoder input
  pure (sum $ scoreRound <$> rounds)

decodeInput :: LineDecoder -> Text -> Maybe [(Hand, Hand)]
decodeInput decoder input = traverse (decodeLine decoder) (T.lines input)

decodeLine :: LineDecoder -> Text -> Maybe (Hand, Hand)
decodeLine decoder line = do
  case T.words line of
    [other, me] -> decoder (other, me)
    _ -> Nothing

lineDecoderPart1 :: LineDecoder
lineDecoderPart1 (other, me) = do
  otherHand <- decodeHandPart1 other
  myHand <- decodeHandPart1 me
  pure (otherHand, myHand)

decodeHandPart1 :: Text -> Maybe Hand
decodeHandPart1 = \case
  "A" -> Just Rock
  "X" -> Just Rock
  "B" -> Just Paper
  "Y" -> Just Paper
  "C" -> Just Scissors
  "Z" -> Just Scissors
  _ -> Nothing

lineDecoderPart2 :: LineDecoder
lineDecoderPart2 (other, me) = do
  otherHand <- decodeHandPart1 other
  expectedResult <- decodeExpectedResult me
  let myHand = chooseMyHand expectedResult otherHand
  pure (otherHand, myHand)

decodeExpectedResult :: Text -> Maybe Result
decodeExpectedResult = \case
  "X" -> Just Lose
  "Y" -> Just Draw
  "Z" -> Just Win
  _ -> Nothing

chooseMyHand :: Result -> Hand -> Hand
chooseMyHand Lose = chooseLosing
chooseMyHand Win = chooseWinning
chooseMyHand Draw = id

chooseWinning :: Hand -> Hand
chooseWinning = \case
  Rock -> Paper
  Paper -> Scissors
  Scissors -> Rock

chooseLosing :: Hand -> Hand
chooseLosing = \case
  Rock -> Scissors
  Paper -> Rock
  Scissors -> Paper

scoreRound :: (Hand, Hand) -> Int
scoreRound hands@(_, me) = scoreOutcome hands + scoreHand me

scoreHand :: Hand -> Int
scoreHand Rock = 1
scoreHand Paper = 2
scoreHand Scissors = 3

scoreOutcome :: (Hand, Hand) -> Int
scoreOutcome = scoreResult . evaluateRound

scoreResult :: Result -> Int
scoreResult Win = 6
scoreResult Draw = 3
scoreResult Lose = 0

evaluateRound :: (Hand, Hand) -> Result
evaluateRound (Scissors, Rock) = Win
evaluateRound (Rock, Paper) = Win
evaluateRound (Paper, Scissors) = Win
evaluateRound (other, me)
  | other == me = Draw
  | otherwise = Lose
