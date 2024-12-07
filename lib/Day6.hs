{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day6 where

import qualified Data.HashSet as Set
import Data.Hashable
import qualified Data.List as L
import Data.Text as T (Text, length, lines, unpack)
import Data.Text.IO as TIO (readFile)
import GHC.Generics (Generic)
import Text.Printf
import Data.Maybe (isNothing)
import Data.Either (isLeft)

type Coord = (Int, Int)

type Bounds = (Int, Int)

type Obstacles = Set.HashSet Coord

data Direction = North | East | South | West deriving (Eq, Enum, Bounded, Generic, Show)

data Guard = Guard {getCoord :: Coord, getDirection :: Direction} deriving (Eq, Generic, Show)

data World = World {getBounds :: Bounds, getObstacles :: Obstacles} deriving (Show)

data Loop = Loop

instance Hashable Direction

instance Hashable Guard

findObject :: Char -> [Text] -> [Coord]
findObject target rows =
  let findOne line = fst <$> filter (\(_, c) -> c == target) (L.zip [0 ..] $ unpack line)
      processLine (i, line) = (,i) <$> findOne line
      numberedLines = L.zip [0 ..] rows
   in concatMap processLine numberedLines

findObstacles :: [Text] -> Obstacles
findObstacles = Set.fromList . findObject '#'

findGuard :: [Text] -> Maybe Coord
findGuard rows = case findObject '^' rows of
  [c] -> Just c
  _ -> Nothing

buildWorld :: [Text] -> Either Text (World, Guard)
buildWorld rows = case findGuard rows of
  Just c ->
    Right
      ( World
          { getBounds = (T.length (L.head rows), L.length rows),
            getObstacles = findObstacles rows
          },
        Guard c North
      )
  Nothing -> Left "Could not find the guard's initial position"

rotate :: Direction -> Direction
rotate d = case d of
  West -> North
  _ -> succ d

nextStep :: Guard -> Coord
nextStep (Guard (x, y) d) = case d of
  North -> (x, y - 1) -- the origin is at the top of the screen
  South -> (x, y + 1)
  East -> (x + 1, y)
  West -> (x - 1, y)

outOfBounds :: World -> Coord -> Bool
outOfBounds world (x, y) =
  let (h, w) = getBounds world
   in x < 0 || x > w || y < 0 || y > h

clash :: World -> Coord -> Bool
clash w c = Set.member c $ getObstacles w

move :: World -> Guard -> Maybe Guard
move world g@(Guard c d) =
  let c' = nextStep g
   in if outOfBounds world c'
        then Nothing
        else
          if clash world c'
            then Just $ Guard c $ rotate d
            else Just $ Guard c' d

findPath :: World -> PathTracker -> Guard -> Either Loop PathTracker
findPath world steps g = case move world g of
  Nothing -> let path = reverse $ getPath steps
              in Right steps {getPath = path}
  Just g' -> if Set.member g' $ getGuard steps
             then Left Loop
             else findPath world (update steps g') g'

straightPath :: World -> [Guard] -> Guard -> [Guard]
straightPath w p g@(Guard _ d) =
  let c' = nextStep g
      g' = Guard c' d
   in if not (outOfBounds w c' || clash w c')
        then straightPath w (g' : p) g'
        else p

willLoop :: World -> PathTracker -> Guard -> Bool
willLoop w p (Guard c d) =
  let turned = Guard c $ rotate d
  in isLeft $ findPath w p turned

data PathTracker = PathTracker
  { 
    getPath :: [Guard],
    getGuard :: Set.HashSet Guard,
    getUsed :: Set.HashSet Coord
  }



emptyPathTracker :: PathTracker
emptyPathTracker = PathTracker {getPath = [], getGuard=Set.empty, getUsed = Set.empty}

initialPathTracker :: Guard -> PathTracker
initialPathTracker  = update emptyPathTracker 

update :: PathTracker -> Guard -> PathTracker
update (PathTracker p g u) g' = PathTracker (g':p) (Set.insert g' g) (Set.insert (getCoord g') u)

occupied :: PathTracker -> Coord -> Bool
occupied pt c = Set.member c (getUsed pt)

countLoops :: World -> PathTracker -> Int -> [Guard] -> Int
countLoops _ _ c [] = c
countLoops w pt c (h : t) =
  let pt' = update pt h
      n = nextStep h
      c' =
        if not (outOfBounds w n) && not (occupied pt n) && willLoop w pt h
          then c + 1
          else c
   in countLoops w pt' c' t

removeTurns :: [Guard] -> [Guard]
removeTurns [] = []
removeTurns [a] = [a]
removeTurns (a : b : t) =
  if getCoord a == getCoord b
    then b : removeTurns t
    else a : removeTurns (b : t)

findPath' :: World -> Guard -> Either Loop PathTracker
findPath' w g = findPath w (initialPathTracker g) g

runDay6Tasks :: IO ()
runDay6Tasks = do
  let fileName = "data/day6/input.txt"
  fileText <- TIO.readFile fileName
  case buildWorld $ T.lines fileText of
    Right (world, start) -> 
       case findPath' world start of 
          Right path -> do
            printf "Task 1: %d\n" $ Set.size $ getUsed path
            printf "Task 2: %d\n" $ countLoops world emptyPathTracker 0 $ getPath path
          Left Loop -> putStrLn "Couldn't find a non-looping path"
    Left msg -> putStrLn $ unpack msg
