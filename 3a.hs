module Main where

import System.Environment;

main :: IO ()
main = do
    args <- getArgs
    rawData <- readFile $ head args
    print $ solve $ lines rawData

solve :: [String] -> Int
solve report =
    gamma * epsilon
    where
        gamma = binToDecimal gammaBin
        epsilon = binToDecimal $ negateBin gammaBin
        gammaBin = calcGamma bitCount $ length report
        bitCount = sumCorrespondingBits $ map toBitList report

toBitList :: String -> [Int]
toBitList = map (read . (:[]) :: Char -> Int)

sumCorrespondingBits :: [[Int]] -> [Int]
sumCorrespondingBits xs = foldl1 (zipWith (+)) xs

calcGamma :: [Int] -> Int -> [Int]
calcGamma bitCountList max = map (\c -> if c * 2 > max then 1 else 0) bitCountList

negateBin :: [Int] -> [Int]
negateBin = map negateBit
    where
        negateBit 1 = 0
        negateBit 0 = 1

binToDecimal :: [Int] -> Int
binToDecimal = foldl (\dec bit -> dec * 2 + bit) 0
