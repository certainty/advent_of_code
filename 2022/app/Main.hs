{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified AOC.Day1 as Day1
import qualified AOC.Day2 as Day2
import qualified AOC.Day3 as Day3
import qualified AOC.Day4 as Day4
import Options.Applicative

data Options = Options
  { _day1 :: Bool,
    _day2 :: Bool,
    _day3 :: Bool,
    _day4 :: Bool
  }

parseOpts :: Parser Options
parseOpts =
  Options
    <$> switch (long "day1" <> help "Run day 1")
    <*> switch (long "day2" <> help "Run day 2")
    <*> switch (long "day3" <> help "Run day 3")
    <*> switch (long "day4" <> help "Run day 4")

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (parseOpts <**> helper) (fullDesc <> progDesc "Run Advent of Code 2020")

run :: Options -> IO ()
run Options {..}
  | _day1 = Day1.run
  | _day2 = Day2.run
  | _day3 = Day3.run
  | _day4 = Day4.run
  | otherwise = putStrLn "No day"
