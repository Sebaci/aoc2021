module Main where

import Data.List
import System.Environment

main = do
  args <- getArgs
  input <- readFile $ head args
  print $ solve $ parseInput input

solve :: [([String], [String])] -> Int
solve = length . filter isUnique . concatMap snd
  where
    isUnique number = length number `elem` uniqueLengths
    uniqueLengths = [2, 3, 4, 7] -- unique lengths for numbers 1,7,4,8

parseInput :: String -> [([String], [String])]
parseInput = map parseLine . lines
  where
    parseLine line = (words patternsString, words digitsString)
      where
        (patternsString, _ : digitsString) = span (/= '|') line
