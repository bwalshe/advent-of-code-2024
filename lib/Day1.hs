{-# LANGUAGE OverloadedStrings #-}

module Day1 where

import qualified Data.HashMap.Strict as Map
import Data.List (sort)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Printf
import Util

pairParser :: Parser (Int, Int)
pairParser = do
  a <- L.decimal
  _ <- space
  b <- L.decimal
  return (a, b)

dataParser :: Parser ([Int], [Int])
dataParser = unzip <$> many (pairParser <* newline)

sumDifference :: [Int] -> [Int] -> Int
sumDifference numsA numsB =
  sum $ abs . uncurry (-) <$> zip (sort numsA) (sort numsB)

countNums :: [Int] -> Map.HashMap Int Int
countNums = foldl (flip incrementCount) Map.empty
  where
    incrementCount n = Map.insertWith (+) n 1

task2 :: [Int] -> [Int] -> Int
task2 numsA numsB =
  let counts = countNums numsB
   in sum $ fmap (\n -> n * Map.findWithDefault 0 n counts) numsA

makeLists :: [(Int, Int)] -> ([Int], [Int])
makeLists = unzip

tasks :: ([Int], [Int]) -> IO ()
tasks pairs = do
  printf "Task 1: %d\n" $ uncurry sumDifference pairs
  printf "Task 2: %d\n" $ uncurry task2 pairs

runDay1Tasks :: IO ()
runDay1Tasks = runDailyTasks dataParser tasks "data/day1/input1.txt"
