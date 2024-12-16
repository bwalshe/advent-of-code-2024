{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day16 where

import Data.HashMap.Strict as Map
import Data.HashSet as Set
import Data.Hashable (Hashable)
import Data.List as L
import Data.PSQueue as PSQueue
import Data.Text as T (Text, lines, unpack)
import Data.Text.IO as TIO
import GHC.Generics (Generic)
import Text.Printf

type Coord = (Int, Int)

data Maze = Maze {getCoords :: HashSet Coord, getEnd :: Coord}

contains :: Maze -> Coord -> Bool
contains m c = Set.member c $ getCoords m

isEnd :: Maze -> Coord -> Bool
isEnd m c = c == getEnd m

remove :: Maze -> Coord -> Maze
remove (Maze m e) c = Maze {getCoords = Set.delete c m, getEnd = e}

data Direction = N | E | S | W deriving (Eq, Show, Enum, Bounded, Generic, Ord)

instance Hashable Direction

turnLeft :: Direction -> Direction
turnLeft d
  | d == N = W
  | otherwise = pred d

turnRight :: Direction -> Direction
turnRight d
  | d == W = N
  | otherwise = succ d

move :: Direction -> Coord -> Coord
move N (i, j) = (i - 1, j)
move E (i, j) = (i, j + 1)
move S (i, j) = (i + 1, j)
move W (i, j) = (i, j - 1)

unmove :: Direction -> Coord -> Coord
unmove N = move S
unmove E = move W
unmove S = move N
unmove W = move E

addCoords :: [Text] -> [(Coord, Char)]
addCoords rows = concatMap doRow $ zip [0 ..] rows
  where
    doRow (i, row) = (\(j, c) -> ((i, j), c)) <$> zip [0 ..] (unpack row)

buildMaze :: [Text] -> Either Text (Maze, Coord)
buildMaze rows =
  let filterChar c cs = fst <$> L.filter (\(_, c') -> c == c') cs
      rawCoords = addCoords rows
      coords = Set.fromList $ filterChar '.' rawCoords
      start = case filterChar 'S' rawCoords of
        [c] -> Right c
        [] -> Left "Couldn't find start point"
        _ -> Left "Found multiple start points"
      end = case filterChar 'E' rawCoords of
        [c] -> Right c
        [] -> Left "Couldn't find end point"
        _ -> Left "Found multiple end points"
   in (\c s e -> (Maze (Set.insert s c) e, s)) <$> Right coords <*> start <*> end

data Distance = Infinity | Dist Int deriving (Show, Eq)

addDistance :: Distance -> Distance -> Distance
addDistance Infinity _ = Infinity
addDistance _ Infinity = Infinity
addDistance (Dist a) (Dist b) = Dist $ a + b

instance Ord Distance where
  Infinity `compare` Infinity = EQ
  Infinity `compare` _ = GT
  _ `compare` Infinity = LT
  (Dist a) `compare` (Dist b) = a `compare` b

type CostedSteps = HashMap (Coord, Direction) Int

type Step = (Coord, Direction)

type DistanceMap = HashMap Step Distance

type DistanceQueue = PSQ Step Distance

getDistance :: DistanceMap -> Step -> Distance
getDistance m s = Map.findWithDefault Infinity s m

neighboursBack :: Step -> [Step]
neighboursBack (c, d) = [(unmove d c, d), (c, turnRight d), (c, turnLeft d)]

neighbourCosts :: DistanceMap -> Step -> [(Step, Distance)]
neighbourCosts m s@(_, d) =
  let pick = flip Map.member m
      cost (_, d') = if d' == d then 1 else 1000
      costed s' = (s', addDistance (getDistance m s) (Dist $ cost s'))
   in costed <$> L.filter pick (neighboursBack s)

type DistanceState = (DistanceMap, DistanceQueue)

updateStep :: DistanceState -> (Step, Distance) -> DistanceState
updateStep (m, q) (s, d) = (m', q')
  where
    m' = Map.adjust (min d) s m
    q' = PSQueue.adjust (min d) s q

shortestPath :: DistanceState -> DistanceState
shortestPath (m, q) = case PSQueue.minView q of
  Just (b, q') ->
    let s = PSQueue.key b
     in shortestPath $ L.foldl updateStep (m, q') (neighbourCosts m s)
  Nothing -> (m, q)

findShortestPaths :: Maze -> DistanceMap
findShortestPaths (Maze coords end) =
  let allDirections c = (c,) <$> enumFrom N
      steps = concatMap allDirections (Set.toList coords)
      distances = Map.fromList $ (,Infinity) <$> steps
      distances' = L.foldl (\m k -> Map.insert k (Dist 0) m) distances (allDirections end)
      queue = PSQueue.fromList $ uncurry (:->) <$> Map.toList distances'
   in fst $ shortestPath (distances', queue)

task1 :: DistanceMap -> Coord -> Maybe Distance
task1 m s = Map.lookup (s, E) m

findNeighboursOnPath :: DistanceMap -> Step -> [Step]
findNeighboursOnPath m (c, d) = fst <$> L.filter pick neighbours
  where
    dist = getDistance m (c, d)
    pick (s', d') = getDistance m s' == addDistance dist (Dist d')
    neighbours = [((move d c, d), -1), ((c, turnLeft d), -1000), ((c, turnRight d), -1000)]

findPaths :: DistanceMap -> Step -> [Step]
findPaths m s = s : concatMap (findPaths m) neighbours
  where
    neighbours = findNeighboursOnPath m s

task2 :: DistanceMap -> Coord -> Int
task2 m s = Set.size $ Set.fromList $ fst <$> findPaths m (s, E)

runDay16Tasks :: IO ()
runDay16Tasks =
  do
    rows <- T.lines <$> TIO.readFile "data/day16/input.txt"
    printf "Read %d lines\n" $ L.length rows
    case buildMaze rows of
      Right (maze, s) -> do
        printf "start is %s end is %s\n" (show s) (show $ getEnd maze)
        let distances = findShortestPaths maze
        printf "Task 1: %s\n" $ show $ task1 distances s
        printf "Task 2: %s\n" $ show $ task2 distances s
      Left e -> printf "%s\n" e
