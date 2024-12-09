{-# LANGUAGE TupleSections #-}

module Day8 where

import Data.Bifunctor (second)
import Data.HashMap.Strict as Map (HashMap, fromListWith, toList)
import qualified Data.HashSet as Set
import Data.List as L (length)
import Data.Text as T (Text, length, lines, unpack)
import qualified Data.Text.IO as TIO
import Text.Printf

type Coord = (Int, Int)

type Bound = Coord

addCoords :: [Text] -> [(Char, Coord)]
addCoords rows =
  let addCoordsRow rowNum row = (\(i, c) -> (c, (i, rowNum))) <$> zip [0 ..] (unpack row)
   in concatMap (uncurry addCoordsRow) $ zip [0 ..] rows

parseRows :: [Text] -> Map.HashMap Char [Coord]
parseRows rows =
  let coords = second (: []) <$> filter ((/= '.') . fst) (addCoords rows)
   in Map.fromListWith (++) coords

parseData :: [Text] -> (Bound, Map.HashMap Char [Coord])
parseData rows =
  let w = T.length $ head rows
      h = L.length rows
      antennas = parseRows rows
   in ((w, h), antennas)

expandCoords :: Bound -> Coord -> Coord -> [Coord]
expandCoords bds (x1, y1) (x2, y2) =
  let dx = x2 - x1
      dy = y2 - y1
   in filter (inbounds bds) [(x1 - dx, y1 - dy), (x2 + dx, y2 + dy)]

allPairs :: [Coord] -> [(Coord, Coord)]
allPairs [] = []
allPairs [_] = []
allPairs (h : t) = ((,h) <$> t) ++ allPairs t

inbounds :: Bound -> Coord -> Bool
inbounds (xmax, ymax) (x, y) = x >= 0 && x < xmax && y >= 0 && y < ymax

type AntinodeFn = Coord -> Coord -> [Coord]

findAllAntinodes :: AntinodeFn -> [[Coord]] -> [Coord]
findAllAntinodes afn coords =
  let pairs = concatMap allPairs coords
   in concatMap (uncurry afn) pairs

task1 :: Bound -> Map.HashMap Char [Coord] -> Int
task1 bds coords =
  let groupedAntennas = snd <$> Map.toList coords
      antinodes = findAllAntinodes (expandCoords bds) groupedAntennas
   in Set.size $ Set.fromList antinodes

task2 :: Bound -> Map.HashMap Char [Coord] -> Int 
task2 bds coords =
  let groupedAntennas = snd <$> Map.toList coords
      antinodes = findAllAntinodes (expandRay bds) groupedAntennas
   in Set.size $ Set.fromList antinodes

expandRay :: Bound -> Coord -> Coord -> [Coord]
expandRay bds (x1, y1) (x2, y2) =
  let dx = x2 - x1
      dy = y2 - y1
      scale = gcd dx dy
      dx' = dx `div` scale
      dy' = dy `div` scale
      ray1 = (\i -> (x1 + dx' * i, y1 + dy' * i)) <$> [0 ..]
      ray2 = (\i -> (x1 - dx' * i, y1 - dy' * i)) <$> [1 ..]
   in takeWhile (inbounds bds) ray1 ++ takeWhile (inbounds bds) ray2

runDay8Tasks :: IO ()
runDay8Tasks = do
  let fileName = "data/day8/input.txt"
  rows <- T.lines <$> TIO.readFile fileName
  let (bds, coords) = parseData rows
  printf "Read %d lines\n" $ L.length rows
  printf "Bounds are %s \n\n" $ show bds
  printf "Task 1: %d\n" $ task1 bds coords
  printf "Task 2: %d\n" $ task2 bds coords
