{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified AOC.Day1 as Day1
import Options.Applicative

data Options = Options
  { _day1 :: Bool
  }

parseOpts :: Parser Options
parseOpts =
  Options
    <$> switch (long "day1" <> help "Run day 1")

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (parseOpts <**> helper) (fullDesc <> progDesc "Run Advent of Code 2020")

run :: Options -> IO ()
run Options {..}
  | _day1 = Day1.run
  | otherwise = putStrLn "No day"
