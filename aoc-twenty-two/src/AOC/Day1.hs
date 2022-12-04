module AOC.Day1 (run, solve) where

import qualified Data.List.Split as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Read

run :: IO ()
run = do
  input <- TIO.readFile "data/day1.txt"
  case solve input of
    Left err -> print err
    Right n -> print n

solve :: Text -> Either String Int
solve input = do
  let caloriesTextual = S.splitWhen (== "") (T.lines input)
  caloriesNumeric <- traverse (traverse (\t -> fst <$> decimal t)) caloriesTextual
  pure $ maximum $ sum <$> caloriesNumeric