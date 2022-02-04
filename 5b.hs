{-# LANGUAGE TupleSections #-}

module Main where

import Data.List
import qualified Data.Map.Strict as Map
import System.Environment

type Point = (Int, Int)

type Line = (Point, Point)

main :: IO ()
main = do
  args <- getArgs
  input <- readFile $ head args
  print $ solve $ map parseLine $ lines input

solve :: [Line] -> Int
solve = Map.size . Map.filter (> 1) . allPointsMap
  where
    allPointsMap = Map.unionsWith (+) . map lineToPointsMap
    lineToPointsMap = Map.fromList . (map (,1) . linePoints)

parseLine :: String -> Line
parseLine line = (toPoint from, toPoint to)
  where
    toPoint str = (read rawX, read rawY)
      where
        (rawX, _ : rawY) = splitAt comma str
        (Just comma) = elemIndex ',' str
    [from, _, to] = words line

linePoints :: Line -> [Point]
linePoints ((x1, y1), (x2, y2))
  | x1 == x2 || y1 == y2 = [(x, y) | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2]]
  | otherwise = zip (axisLine x1 x2) (axisLine y1 y2)
  where
    axisLine a b = if a < b then [a .. b] else reverse [b .. a]
