module Day7 where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Printf
import Util

type Row = (Integer, [Integer])

rowParser :: Parser Row
rowParser = do
  target <- L.decimal
  _ <- char ':'
  numbers <- many $ char ' ' *> L.decimal
  return (target, numbers)

dataParser :: Parser [Row]
dataParser = endBy rowParser eol

combos :: [Integer -> Integer -> Integer] -> [Integer] -> [Integer]
combos ops nums =
  let combos' acc [] = acc
      combos' acc (h : t) =
        let acc' = (ops <*> acc ) <*> pure h -- apply each op to h and each elt of acc and concat the result
         in combos' acc' t
   in combos' [head nums] $ tail nums

checkValid1 :: Row -> Bool
checkValid1 (target, nums) = target `elem` combos [(+), (*)] nums

sumValidRows :: (Row -> Bool) -> [Row] -> Integer
sumValidRows check rows = sum $ fst <$> filter check rows

shifts :: Integer -> Integer
shifts 0 = 0
shifts n = 1 + shifts (n `div` 10)

combineDigits :: Integer -> Integer -> Integer
combineDigits a b = a * (10 ^ shifts b) + b

checkValid2 :: Row -> Bool
checkValid2 (target, nums) = target `elem` combos [(+), (*), combineDigits] nums

tasks :: [Row] -> IO ()
tasks rows = do
  printf "Read %d rows\n" $ length rows
  printf "Task 1: %d\n" $ sumValidRows checkValid1 rows
  printf "Task 2: %d\n" $ sumValidRows checkValid2 rows

runDay7Tasks :: IO ()
runDay7Tasks = runDailyTasks dataParser tasks "data/day7/input.txt"
