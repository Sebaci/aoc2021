{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Data.Function (on)
import Data.List
import System.Environment

type Position = Int

type Count = Int

type Cost = Int

data PositionState = PositionState
  { position :: Position,
    leftCount :: Int,
    cost :: Cost,
    rightCount :: Int,
    remainingCrabs :: [(Position, Count)]
  }

main :: IO ()
main = do
  args <- getArgs
  input <- readFile $ head args
  print $ solve $ parseInput input

solve :: [Position] -> Position
solve crabs = cost $ minimumBy (compare `on` cost) possibleAlignmentStates
  where
    possibleAlignmentStates =
      scanl calculateNextPositionState firstPositionState [minPos + 1 .. maxPos]

    firstPositionState =
      PositionState
        { position = minPos,
          leftCount = leftCount,
          cost = sum crabs - minPos * length crabs,
          rightCount = length crabs - leftCount,
          remainingCrabs = cs
        }

    leftCount = snd c

    minPos = head sortedCrabs
    maxPos = last sortedCrabs

    (c : cs) = groupCrabs sortedCrabs
    sortedCrabs = sort crabs

calculateNextPositionState :: PositionState -> Position -> PositionState
calculateNextPositionState
  positionState@PositionState
    { position,
      leftCount,
      cost,
      rightCount,
      remainingCrabs = ((crabPosition, crabCount) : cs)
    }
  currentPosition
    | currentPosition < crabPosition =
      positionState
        { position = currentPosition,
          cost = cost + leftCount - rightCount
        }
    | otherwise =
      positionState
        { position = currentPosition,
          leftCount = leftCount + crabCount,
          cost = cost + leftCount - rightCount,
          rightCount = rightCount - crabCount,
          remainingCrabs = cs
        }

groupCrabs :: [Position] -> [(Position, Count)]
groupCrabs crabs = map (\posCrabs -> (head posCrabs, length posCrabs)) $ group crabs

parseInput :: String -> [Position]
parseInput =
  map read . unfoldr (\s -> if null s then Nothing else Just $ span (/= ',') (dropWhile (== ',') s))
