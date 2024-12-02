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

runDay2Tasks :: IO ()
runDay2Tasks = do
  let fileName = "data/day2/input.txt"
  fileText <- TIO.readFile fileName
  case runParser dataParser fileName fileText of
    Right lists -> do
      printf "Task 1: %d\n" $ task1 lists
      printf "Task 2: %d\n" $ task2 lists
    Left e -> print $ errorBundlePretty e
