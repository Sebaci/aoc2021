module Main where

import Data.List
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  rawData <- readFile $ head args
  print $ solve $ lines rawData

solve :: [String] -> Int
solve report =
  oxygenRating * scrubberRating
  where
    oxygenRating = binToDecimal $ computeRating mostCommonLeadingBit numbers
    scrubberRating = binToDecimal $ computeRating leastCommonLeadingBit numbers
    numbers = map toBitList report

computeRating :: ([[Int]] -> Int) -> [[Int]] -> [Int]
computeRating nextBitStrategy = unfoldr filterNumbersStep
  where
    filterNumbersStep [[]] = Nothing
    filterNumbersStep [bit : number] = Just (bit, [number])
    filterNumbersStep numbers = Just (nextBit, (map tail . filterByLeadingBit nextBit) numbers)
      where
        nextBit = nextBitStrategy numbers

mostCommonLeadingBit :: [[Int]] -> Int
mostCommonLeadingBit numbers = if countLeadingOnes numbers * 2 >= length numbers then 1 else 0
  where
    countLeadingOnes = sum . map head

leastCommonLeadingBit :: [[Int]] -> Int
leastCommonLeadingBit numbers = if mostCommonLeadingBit numbers == 1 then 0 else 1

filterByLeadingBit :: Int -> [[Int]] -> [[Int]]
filterByLeadingBit b = filter $ (== b) . head

toBitList :: String -> [Int]
toBitList = map (read . (: []) :: Char -> Int)

binToDecimal :: [Int] -> Int
binToDecimal = foldl1 $ (+) . (* 2)
