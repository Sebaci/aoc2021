module Main where

import Control.Applicative (liftA2)
import Data.List (transpose)
import Data.Maybe (catMaybes)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  rawData <- readFile $ head args
  print $ solve $ map (map (read . (: []))) $ lines rawData

solve :: [[Int]] -> Int
solve mat = foldl (((+ 1) .) . (+)) 0 (concatMap catMaybes mins)
  where
    mins = zipWith (zipWith (liftA2 ((fst .) . (,)))) horizontalMins verticalMins

    verticalMins = transpose $ map findRowMins $ transpose mat
    horizontalMins = map findRowMins mat

    findRowMins = findLowestPoints . toNeighboursList

toNeighboursList :: [Int] -> [(Int, Int, Int)]
toNeighboursList xs = zip3 (10 : init xs) xs (tail xs ++ [10])

findLowestPoints :: [(Int, Int, Int)] -> [Maybe Int]
findLowestPoints = map takeIfLowest
  where
    takeIfLowest (left, current, right)
      | current < left && current < right = Just current
      | otherwise = Nothing
