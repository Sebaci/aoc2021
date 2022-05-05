{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad.State
import Data.Array
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.List (sortOn)
import Data.Ord (Down (Down))
import Data.Sequence (Seq (..), singleton, (|>))
-- import Data.Sequence
import System.Environment

data Check = Checked | Unchecked

data Kind = Basin | Top

-- data Location = Location Kind Check Height
data Location = Location {kind :: Kind, check :: Check, height :: Int}

type Coords = (Int, Int)

data FindBasinState = FindBasinState
  { searchQueue :: Seq Coords,
    basin :: [Location],
    heightmap :: Array Coords Location
  }

main :: IO ()
main = do
  args <- getArgs
  rawData <- readFile $ head args
  print $ solve $ map (map (read . (: []))) $ lines rawData

solve :: [[Int]] -> Int
solve mat = biggest3Product basins
  where
    biggest3Product = product . take 3 . sortOn Down . map length
    basins = map (evalState findBasin . toInitialState) basinLows

    toInitialState coords =
      FindBasinState
        { searchQueue = singleton coords,
          basin = [heightmap ! coords],
          heightmap = heightmap // [(coords, (heightmap ! coords) {check = Checked})]
        }
    basinLows = findLowPoints heightmap
    heightmap = toLocationsArray mat

findLowPoints :: Array Coords Location -> [Coords]
findLowPoints area = filter isLow allCoords
  where
    allCoords = indices area
    isLow coords = all (((<) `on` height) loc . (area !)) adjecent
      where
        loc = area ! coords
        adjecent = adjecentCoords (bounds area) coords

toLocationsArray :: [[Int]] -> Array Coords Location
toLocationsArray xs = listArray ((1, 1), (height, width)) (concatMap (map toLocation) xs)
  where
    height = length xs
    width = length $ head xs

    toLocation height
      | height == 9 = Location Top Unchecked height
      | otherwise = Location Basin Unchecked height

findBasin :: State FindBasinState [Location]
findBasin = do
  FindBasinState {searchQueue, basin, heightmap} <- get
  case searchQueue of
    Empty -> return basin
    c :<| queue ->
      modify setQueue >> traverse_ checkLocation adjecent >> findBasin
      where
        setQueue = (\st -> st {searchQueue = queue})
        adjecent = adjecentCoords (bounds heightmap) c

checkLocation :: Coords -> State FindBasinState ()
checkLocation coords = do
  FindBasinState {searchQueue, basin, heightmap} <- get
  case heightmap ! coords of
    Location _ Checked _ -> return ()
    Location Top _ _ -> return ()
    location ->
      put
        FindBasinState
          { searchQueue = searchQueue |> coords,
            basin = location : basin,
            heightmap = heightmap // [(coords, location {check = Checked})]
          }

adjecentCoords :: (Coords, Coords) -> Coords -> [Coords]
adjecentCoords ((yMin, xMin), (yMax, xMax)) (y, x) = filter withinBounds adjecent
  where
    adjecent = [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]
    withinBounds (y, x) = y >= yMin && y <= yMax && x >= xMin && x <= xMax
