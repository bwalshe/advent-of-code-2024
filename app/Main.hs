{-# LANGUAGE OverloadedStrings #-} 
module Main where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Text.Megaparsec.Char
import Text.Megaparsec
import Data.Void 
import Data.List
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.HashMap.Strict as Map


type Parser = Parsec Void Text

pairParser :: Parser (Int, Int)
pairParser = do
   a <- L.decimal
   _ <- space
   b <- L.decimal
   return (a, b)

dataParser :: Parser [(Int, Int)]
dataParser = many (pairParser <* newline)

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
   in
      sum $ fmap (\n -> n * Map.findWithDefault 0 n counts) numsA

main :: IO ()
main = do 
     let fileName = "data/day1/input1.txt"
     fileText <- TIO.readFile fileName
     case runParser dataParser fileName fileText of
        Right pairs -> print $ task2 numsA numsB
           where (numsA, numsB) = unzip pairs
        Left e -> putStrLn $ errorBundlePretty e

