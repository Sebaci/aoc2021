module Main where

import Data.List
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  input <- readFile $ head args
  print $ solve $ parseInput input

solve :: [Int] -> Int
solve state = length $ iterate nextDay state !! 80

nextDay :: [Int] -> [Int]
nextDay = foldr (\t s -> if t == 0 then 6 : 8 : s else (t -1) : s) []

parseInput :: String -> [Int]
parseInput = unfoldr f
  where
    f s
      | null s = Nothing
      | otherwise = Just (read $ takeWhile (/= ',') s, dropWhile (== ',') $ dropWhile (/= ',') s)