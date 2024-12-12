{-# LANGUAGE TupleSections #-}

module Day12 where

import Control.Monad.State.Lazy
import Data.HashMap.Strict as Map
import Data.HashSet as Set
import Data.List as L
import Data.Text as T (Text, lines, unpack)
import Data.Text.IO as TIO
import Text.Printf

type Coord = (Int, Int)

type Plant = Char

type PlantMap = Map.HashMap Coord Plant

type PlantMapItem = (Coord, Plant)

type Region = Set.HashSet Coord

emptyRegion :: Region
emptyRegion = Set.empty

-- Today I'm going with i down j across.
-- this feels more natural when indexing things
addCoords :: [Text] -> PlantMap
addCoords rows =
  let addCoordsRow i row = (\(j, c) -> ((i, j), c)) <$> zip [0 ..] (unpack row)
   in Map.fromList $ concatMap (uncurry addCoordsRow) $ zip [0 ..] rows

pop :: PlantMap -> Maybe (PlantMapItem, PlantMap)
pop m
  | Map.null m = Nothing
  | otherwise =
      let (c, p) = head $ Map.toList m
       in Just ((c, p), Map.delete c m)

neighbours :: Coord -> [Coord]
neighbours (i, j) = [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)]

takeNeighbours :: PlantMap -> PlantMapItem -> ([PlantMapItem], PlantMap)
takeNeighbours m (c, p) =
  let matches = L.filter (\c' -> Map.lookup c' m == Just p) $ neighbours c
      m' = L.foldl (flip Map.delete) m matches
   in ((,p) <$> matches, m')

expandRegion :: (PlantMap, Region) -> PlantMapItem -> (PlantMap, Region)
expandRegion (m, r) (c, p) = case takeNeighbours m (c, p) of
  ([], _) -> (m, Set.insert c r)
  (ns, m') -> L.foldl expandRegion (m', Set.insert c r) ns

type RegionState = (PlantMap, [Region])

allRegions :: State RegionState [Region]
allRegions = do
  (m, rs) <- get
  case pop m of
    Just (start@(c, _), m') ->
      do
        let (m'', r) = expandRegion (m', Set.singleton c) start
        put (m'', r : rs)
        allRegions
    Nothing -> return rs

area :: Region -> Int
area = Set.size

perimiter :: Region -> Int
perimiter r = sum $ sides <$> Set.toList r
  where
    sides c = 4 - Set.size (Set.intersection r $ Set.fromList $ neighbours c)

task1 :: PlantMap -> Int
task1 m = sum $ price <$> evalState allRegions (m, [])
  where
    price x = area x * perimiter x

findEdgeFacing :: (Coord -> Coord) -> Region -> Region
findEdgeFacing f r = Set.filter (\c -> not $ Set.member (f c) r) r

findStartHorizontal :: Coord -> Region -> Coord
findStartHorizontal c@(i, j) r = if Set.member c' r then findStartHorizontal c' r else c
  where c' = (i, j-1)


removeHorizontal :: Coord -> Region -> Region
removeHorizontal c@(i, j) r = if Set.member c' r' then removeHorizontal c' r' else r'
  where
    c' = (i, j+1)
    r' = Set.delete c r

countHorizontal :: Region -> Int
countHorizontal r
  | Set.null r = 0
  | otherwise = 1 + countHorizontal (removeHorizontal start r)
  where
    start = findStartHorizontal (head $ Set.toList r) r

findStartVertical :: Coord -> Region -> Coord
findStartVertical c@(i, j) r = if Set.member c' r then findStartVertical c' r else c
   where c' = (i-1, j)

removeVertical :: Coord -> Region -> Region
removeVertical c@(i, j) r = if Set.member c' r' then removeVertical c' r' else r'
  where
    c' = (i + 1, j)
    r' = Set.delete c r

countVertical :: Region -> Int
countVertical r
  | Set.null r = 0
  | otherwise = 1 + countVertical (removeVertical start r)
  where
    start = findStartVertical (head $ Set.toList r) r

countSides :: Region -> Int
countSides r = countHorizontal north + countHorizontal south + countVertical east + countVertical west
   where
      north = findEdgeFacing (\(i, j) -> (i-1, j)) r
      south = findEdgeFacing (\(i, j) -> (i+1, j)) r
      east = findEdgeFacing (\(i, j) -> (i, j+1)) r
      west = findEdgeFacing (\(i, j) -> (i, j-1)) r

task2 :: PlantMap -> Int
task2 m =  sum $ price <$> evalState allRegions (m, [])
  where
    price x = area x * countSides x

runDay12Tasks :: IO ()
runDay12Tasks = do
  let fileName = "data/day12/input.txt"
  rows <- T.lines <$> TIO.readFile fileName
  printf "Read %d lines\n" $ L.length rows
  let m = addCoords rows
  printf "Task 1: %d\n" $ task1 m
  printf "Task 2: %d\n" $ task2 m
