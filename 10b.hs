module Main where

import Control.Monad (foldM)
import Data.List (sort)
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
solve lines = sort scores !! (length scores `div` 2)
  where
    scores = filter (> 0) $ map lineScore lines

lineScore :: [Bracket] -> Int
lineScore ((O b) : bs) = case foldM checkBracket [b] bs of
  Left _ -> 0
  Right openBrackets -> foldl (\score bracket -> score * 5 + toScore bracket) 0 openBrackets
  where
    checkBracket previous (O cur) = Right $ cur : previous
    checkBracket (last : bs) (C cur)
      | last == cur = Right bs
      | otherwise = Left cur
    checkBracket [] (C cur) = error "No matching opening bracket"
lineScore _ = error "Invalid bracket list"

toScore :: BracketType -> Int
toScore bracket = case bracket of
  Round -> 1
  Square -> 2
  Curly -> 3
  Angle -> 4

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
