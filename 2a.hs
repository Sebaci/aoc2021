module Main where

import System.Environment;

type Command = (String, Int)

main :: IO ()
main = do
    args <- getArgs
    rawData <- readFile $ head args
    print $ solve $ map toCommand $ lines rawData

toCommand :: String -> (String, Int)
toCommand line = (direction, read value)
    where
        [direction, value] = words line

solve :: [Command] -> Int
solve cmds = uncurry (*) $ foldl1 addPairs positionChangeHistory
    where
        addPairs (x, y) (x', y') = (x + x', y + y')
        positionChangeHistory = map toPositionChange cmds

toPositionChange :: Command -> (Int, Int)
toPositionChange ("up", value) = (0, -value)
toPositionChange ("down", value) = (0, value)
toPositionChange ("forward", value) = (value, 0)