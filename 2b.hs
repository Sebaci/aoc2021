{-# LANGUAGE NamedFieldPuns #-}
module Main where

import System.Environment;

import Control.Monad.State;

{-
 here we show how to use state monad to solve the problem
 the state stores information about the submarine and is updated as we move through commands
-}

type Command = (String, Int)

data SubmarineState = SubmarineState
    { horizontal :: Int
    , depth :: Int
    , aim :: Int
    }

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
solve cmds = evalState (moveSubmarine cmds) $ SubmarineState 0 0 0

moveSubmarine :: [Command] -> State SubmarineState Int
moveSubmarine [] = do
    SubmarineState{ horizontal, depth } <- get
    return $ horizontal * depth

moveSubmarine (c:cmds) = do
    submarine@SubmarineState{ horizontal, depth, aim } <- get
    case c of
        ("up", value) -> put $ submarine { aim = aim - value }
        ("down", value) -> put $ submarine { aim = aim + value }
        ("forward", value) -> put $ submarine { horizontal = horizontal + value, depth = depth + (aim * value) }
    moveSubmarine cmds
