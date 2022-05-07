module Main where

import Control.Monad (foldM)
import System.Environment (getArgs)

data BracketType = Round | Square | Curly | Angle
  deriving (Eq)

data Bracket = O BracketType | C BracketType

main :: IO ()
main = do
  args <- getArgs
  rawData <- readFile $ head args
  print $ solve $ map parseLine $ lines rawData

solve :: [[Bracket]] -> Int
solve = sum . map errorScore

errorScore :: [Bracket] -> Int
errorScore ((O b) : bs) = case foldM checkBracket [b] bs of
  Left bracket -> toScore bracket
  Right _ -> 0
  where
    checkBracket previous (O cur) = Right $ cur : previous
    checkBracket (last : bs) (C cur)
      | last == cur = Right bs
      | otherwise = Left cur
    checkBracket [] (C cur) = error "No matching opening bracket"
errorScore _ = error "Invalid bracket list"

toScore :: BracketType -> Int
toScore bracket = case bracket of
  Round -> 3
  Square -> 57
  Curly -> 1197
  Angle -> 25137

parseLine :: String -> [Bracket]
parseLine = map toBracket
  where
    toBracket char = case char of
      '(' -> O Round
      '[' -> O Square
      '{' -> O Curly
      '<' -> O Angle
      ')' -> C Round
      ']' -> C Square
      '}' -> C Curly
      '>' -> C Angle
      _ -> error "invalid character"
