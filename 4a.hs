{-# LANGUAGE TupleSections #-}

module Main where

import Data.List
import System.Environment

data Number = Marked Int | Unmarked Int

type Board = [[Number]]

main :: IO ()
main = do
  args <- getArgs
  input <- readFile $ head args

  let ([rawNumbers] : rawBoards) = splitBy null $ lines input
  let numbers = map read $ splitBy (== ',') rawNumbers :: [Int]
  let bingos = map (map (map (Unmarked . read) . words)) rawBoards
  print $ solve bingos numbers

solve :: [Board] -> [Int] -> Int
solve boards numbers =
  lastCalled * score bingoBoard
  where
    Just (Just lastCalled, bingoBoard) = find (bingo . snd) . concatMap flattenStep $ bingoHistory
    flattenStep (n, boards) = map (n,) boards
    bingoHistory = playBingo boards numbers

playBingo :: [Board] -> [Int] -> [(Maybe Int, [Board])]
playBingo boards = scanl (\(_, b) n -> (Just n, map (markNumberOnBoard n) b)) (Nothing, boards)

markNumberOnBoard :: Int -> Board -> Board
markNumberOnBoard i = map (map f)
  where
    f n@(Unmarked j)
      | i == j = Marked i
      | otherwise = n
    f marked = marked

bingo :: Board -> Bool
bingo b = any (all isMarked) (b ++ transpose b)
  where
    isMarked (Marked _) = True
    isMarked _ = False

score :: Board -> Int
score = sum . concatMap (map numberScore)
  where
    numberScore (Unmarked n) = n
    numberScore _ = 0

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p = unfoldr (\s -> if null s then Nothing else Just (takeWhile (not . p) s, dropWhile p (dropWhile (not . p) s)))
