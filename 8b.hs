module Main where

import Control.Monad (guard)
import Data.List
import System.Environment

type Mask = [String]

type Pattern = String

type Mapping = String

main = do
  args <- getArgs
  input <- readFile $ head args
  print $ solve $ parseInput input

digits :: [[Int]]
digits =
  [ [1, 1, 1, 0, 1, 1, 1],
    [0, 0, 1, 0, 0, 1, 0],
    [1, 0, 1, 1, 1, 0, 1],
    [1, 0, 1, 1, 0, 1, 1],
    [0, 1, 1, 1, 0, 1, 0],
    [1, 1, 0, 1, 0, 1, 1],
    [1, 1, 0, 1, 1, 1, 1],
    [1, 0, 1, 0, 0, 1, 0],
    [1, 1, 1, 1, 1, 1, 1],
    [1, 1, 1, 1, 0, 1, 1]
  ]

toDigitMasks :: String -> [Mask]
toDigitMasks pattern = case length pattern of
  2 -> [mask (digits !! 1) pattern]
  3 -> [mask (digits !! 7) pattern]
  4 -> [mask (digits !! 4) pattern]
  5 ->
    [ mask (digits !! 5) pattern,
      mask (digits !! 2) pattern,
      mask (digits !! 3) pattern
    ]
  6 ->
    [ mask (digits !! 6) pattern,
      mask (digits !! 9) pattern,
      mask (head digits) pattern
    ]
  _ -> [mask (digits !! 8) pattern]
  where
    mask segments pattern = map (\x -> if x == 1 then pattern else anyLetter) segments
    anyLetter = ['a' .. 'g']

solve :: [([String], [String])] -> Int
solve = sum . map decodeEntryDigits

decodeEntryDigits :: ([Pattern], [Pattern]) -> Int
decodeEntryDigits (patterns, digitSegments) = decodeNumber mapping digitSegments
  where
    mapping = findSegmentMapping (map toDigitMasks patterns)

decodeNumber :: Mapping -> [Pattern] -> Int
decodeNumber segmentMapping encodedDigits = foldl (\v d -> v * 10 + d) 0 digitList
  where
    (Just digitList) = mapM decodeDigit encodedDigits
    decodeDigit encodedDigit = elemIndex digit digits
      where
        digit = map (\letter -> if letter `elem` encodedDigit then 1 else 0) segmentMapping

findSegmentMapping :: [[Mask]] -> Mapping
findSegmentMapping = concat . (head . foldl1 combineMasks)

combineMasks :: [Mask] -> [Mask] -> [Mask]
combineMasks masks1 masks2 = do
  m1 <- masks1
  m2 <- masks2

  let merged = mergeMasks m1 m2
  guard $ validMask merged

  return merged

mergeMasks :: Mask -> Mask -> Mask
mergeMasks mask1 mask2 = until (not . knownLettersRepeat) excludeKnownLetters $ zipWith intersect mask1 mask2
  where
    knownLettersRepeat mask = any isKnownLetterRepeated mask
      where
        isKnownLetterRepeated [l] = any (\pattern -> pattern /= [l] && elem l pattern) mask
        isKnownLetterRepeated _ = False

excludeKnownLetters :: Mask -> Mask
excludeKnownLetters mask = map narrowDownSegment mask
  where
    narrowDownSegment segment
      | length segment > 1 = segment \\ knownLetters
      | otherwise = segment
    knownLetters = concat $ filter ((== 1) . length) mask

validMask :: Mask -> Bool
validMask mask = not (any null mask) && knownSegmentsUnique
  where
    knownSegmentsUnique = length (nubBy equalSingletons mask) == length mask
    equalSingletons [x] [y] = x == y
    equalSingletons _ _ = False

parseInput :: String -> [([String], [String])]
parseInput = map parseLine . lines
  where
    parseLine line = (words patternsString, words digitsString)
      where
        (patternsString, _ : digitsString) = span (/= '|') line
