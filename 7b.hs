{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Data.Function (on)
import Data.List
import System.Environment

type Position = Int

type Count = Int

type Cost = Int

type Rate = Int

data PositionState = PositionState
  { costs :: [(Count, Rate)],
    remainingCrabs :: [(Position, Count)]
  }

main :: IO ()
main = do
  args <- getArgs
  input <- readFile $ head args
  print $ solve $ parseInput input

solve :: [Position] -> Cost
solve crabs = minimum alignmentCosts
  where
    alignmentCosts =
      zipWith
        (\s1 s2 -> on (+) calculatePositionCost (costs s1) (costs s2))
        possibleLeftAlignmentStates
        possibleRightAlignmentStates

    possibleLeftAlignmentStates =
      scanl calculateNextPositionState firstPositionState [minPos + 1 .. maxPos]

    possibleRightAlignmentStates =
      scanr (flip calculateNextPositionState) lastPositionState [minPos .. maxPos - 1]

    firstPositionState =
      PositionState
        { costs = [(snd $ head groupedCrabs, 0)],
          remainingCrabs = tail groupedCrabs
        }

    lastPositionState =
      PositionState
        { costs = [(snd $ head reverseGroupedCrabs, 0)],
          remainingCrabs = tail reverseGroupedCrabs
        }

    minPos = head sortedCrabs
    maxPos = last sortedCrabs

    reverseGroupedCrabs = reverse groupedCrabs
    groupedCrabs = groupCrabs sortedCrabs
    sortedCrabs = sort crabs

calculatePositionCost :: [(Count, Rate)] -> Cost
calculatePositionCost = sum . map (\(count, rate) -> count * (rate * (rate + 1)) `div` 2)

calculateNextPositionState :: PositionState -> Position -> PositionState
calculateNextPositionState positionState@PositionState {costs, remainingCrabs} currentPosition
  | currentPosition == crabPosition =
    PositionState
      { costs = (crabCount, 0) : map increaseRate costs,
        remainingCrabs = tail remainingCrabs
      }
  | otherwise =
    PositionState
      { costs = map increaseRate costs,
        remainingCrabs
      }
  where
    (crabPosition, crabCount) = head remainingCrabs
    increaseRate (count, rate) = (count, rate + 1)

groupCrabs :: [Position] -> [(Position, Count)]
groupCrabs = map (\posCrabs -> (head posCrabs, length posCrabs)) . group

parseInput :: String -> [Position]
parseInput =
  map read . unfoldr (\s -> if null s then Nothing else Just $ span (/= ',') (dropWhile (== ',') s))
