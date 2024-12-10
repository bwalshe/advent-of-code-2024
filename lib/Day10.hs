module Day10 where

import Data.HashMap.Strict as Map
import Data.HashSet as Set
import Data.List as L
import Data.Text as T (Text, lines, unpack)
import Data.Text.IO as TIO
import Text.Printf

type Coord = (Int, Int)

type TopoMap = Map.HashMap Coord Int

-- I'm going with x across y down.
-- NOT matrix style i, j
addCoords :: [Text] -> [(Coord, Int)]
addCoords rows =
  let addCoordsRow rowNum row = (\(x, c) -> ((x, rowNum), read [c])) <$> zip [0 ..] (unpack row)
   in concatMap (uncurry addCoordsRow) $ zip [0 ..] rows

parseTable :: [Text] -> TopoMap
parseTable rows = Map.fromList $ addCoords rows

nextSteps :: TopoMap -> Coord -> [Coord]
nextSteps m (x, y) = case Map.lookup (x, y) m of
  Nothing -> []
  Just h -> Map.keys $ Map.filterWithKey test m
    where
      neighbours = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
      test k v = L.elem k neighbours && v == h + 1

getPaths :: TopoMap -> Coord -> [[Coord]]
getPaths m c = case nextSteps m c of
  [] -> [[c]]
  ns ->
    let ts = concatMap (getPaths m) ns
     in (c :) <$> ts

getFullPaths :: TopoMap -> Coord -> [[Coord]]
getFullPaths m c = L.filter (\l -> L.length l == 10) $ getPaths m c

getAllPaths :: TopoMap -> [[Coord]]
getAllPaths m =
  let starts = Map.keys $ Map.filter (== 0) m
   in concatMap (getFullPaths m) starts

task1 :: [[Coord]] -> Int
task1 paths = Set.size $ Set.fromList $ (\l -> (L.head l, L.last l)) <$> paths

task2 :: [[Coord]] -> Int
task2 = Set.size . Set.fromList

runDay10Tasks :: IO ()
runDay10Tasks = do
  let fileName = "data/day10/input.txt"
  rows <- T.lines <$> TIO.readFile fileName
  printf "Read %d lines\n" $ L.length rows
  let trails = getAllPaths $ parseTable rows
  printf "Task 1 %d\n" $ task1 trails
  printf "Task 2 %d\n" $ task2 trails
