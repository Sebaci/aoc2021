module Main where

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  rawData <- readFile $ head args
  print $ solve $ map read $ lines rawData

solve :: [Int] -> Int
solve nums = length $ filter (uncurry (<)) $ zip nums $ tail nums
