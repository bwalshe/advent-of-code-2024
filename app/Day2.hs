{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Printf

type Parser = Parsec Void Text

rowParser :: Parser [Int]
rowParser = sepBy L.decimal (char ' ')

dataParser :: Parser [[Int]]
dataParser = sepBy rowParser newline <* eof

monotonic :: [Int] -> Bool
monotonic (a : b : t)
  | a < b && b <= a + 3 = increasing (b : t)
  | a > b && a <= b + 3 = decreasing (b : t)
  | otherwise = False
monotonic [_] = True
monotonic [] = True

increasing :: [Int] -> Bool
increasing (a : b : t) = a < b && b <= a + 3 && increasing (b : t)
increasing [_] = True
increasing [] = True

decreasing :: [Int] -> Bool
decreasing (a : b : t) = a > b && a <= b + 3 && decreasing (b : t)
decreasing [_] = True
decreasing [] = True

task1 :: [[Int]] -> Int
task1 lists = sum $ map (fromEnum . monotonic) $ filter (not . null) lists

dampenedMonotonic :: [Int] -> Bool
dampenedMonotonic nums = monotonic nums || any monotonic sublists
  where
    exclude n l = take n l ++ drop (n + 1) l
    sublists = [exclude i nums | i <- [0 .. length nums - 1]]

task2 :: [[Int]] -> Int
task2 lists = sum $ map (fromEnum . dampenedMonotonic) $ filter (not . null) lists

runDay2Tasks :: IO ()
runDay2Tasks = do
  let fileName = "data/day2/input.txt"
  fileText <- TIO.readFile fileName
  case runParser dataParser fileName fileText of
    Right lists -> do 
       printf "Task 1: %d\n" $ task1 lists
       printf "Task 2: %d\n" $ task2 lists
    Left e -> print $ errorBundlePretty e
