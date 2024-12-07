{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Printf
import Util

rowParser :: Parser [Int]
rowParser = sepBy L.decimal (char ' ')

dataParser :: Parser [[Int]]
dataParser = sepBy rowParser newline <* eof

data Monotonic = Inc Int | Dec Int | Fail deriving (Eq)

monotonic :: [Int] -> Bool
monotonic (a : b : t) = foldl step start t /= Fail
  where
    isInc a' b' = a' < b' && b' <= a' + 3
    isDec a' b' = a' > b' && a' <= b' + 3
    start
      | isInc a b = Inc b
      | isDec a b = Dec b
      | otherwise = Fail
    step (Inc a') b'
      | isInc a' b' = Inc b'
      | otherwise = Fail
    step (Dec a') b'
      | isDec a' b' = Dec b'
      | otherwise = Fail
    step Fail _ = Fail
monotonic _ = False

task1 :: [[Int]] -> Int
task1 = sum . map (fromEnum . monotonic)

dampenedMonotonic :: [Int] -> Bool
dampenedMonotonic nums = monotonic nums || any monotonic sublists
  where
    exclude n l = take n l ++ drop (n + 1) l
    sublists = [exclude i nums | i <- [0 .. length nums - 1]]

task2 :: [[Int]] -> Int
task2 = sum . map (fromEnum . dampenedMonotonic)

tasks :: [[Int]] -> IO ()
tasks lists = do
  printf "Task 1: %d\n" $ task1 lists
  printf "Task 2: %d\n" $ task2 lists

runDay2Tasks :: IO ()
runDay2Tasks = runDailyTasks dataParser tasks "data/day2/input.txt"
