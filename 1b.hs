module Main where

import Data.List
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  rawData <- readFile $ head args
  print $ solve $ map read $ lines rawData

solve :: [Int] -> Int
solve nums =
  let measurements = map (sum . (take 3)) $ tails nums
   in length $ filter (uncurry (<)) $ zip measurements $ tail measurements
