{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Data.List
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  input <- readFile $ head args
  print $ solve $ parseInput input

solve :: [Int] -> Int
solve initialLanternfish = sum $ iterate nextDay state !! 256
  where
    -- we artificially add one fish per age so state is properly initialized
    -- state keeps fish count per age
    state = map (\c -> length c - 1) $ group $ sort $ [0, 1, 2, 3, 4, 5, 6, 7, 8] ++ initialLanternfish

nextDay :: [Int] -> [Int]
nextDay (zero : rest) = take 6 rest ++ [zero + rest !! 6, rest !! 7, zero]

parseInput :: String -> [Int]
parseInput = unfoldr f
  where
    f s
      | null s = Nothing
      | otherwise = Just (read $ takeWhile (/= ',') s, dropWhile (== ',') $ dropWhile (/= ',') s)
