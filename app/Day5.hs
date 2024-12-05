{-# LANGUAGE OverloadedStrings #-}

module Day5 where

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.List as List (sortBy)
import Text.Megaparsec
import Text.Megaparsec.Char (char, newline, space1)
import Text.Megaparsec.Char.Lexer as L
import Text.Printf
import Util

type PageNumber = Int

type PageOrder = (PageNumber, PageNumber)

pairParser :: Parser PageOrder
pairParser = do
  a <- L.decimal
  _ <- char '|'
  b <- L.decimal
  return (a, b)

pageListParser :: Parser [PageNumber]
pageListParser = sepBy1 L.decimal (char ',')

data BookData = BookData {getOrdering :: [PageOrder], getPageLists :: [[PageNumber]]}

dataParser :: Parser BookData
dataParser = do
  ordering <- label "ordering" $ sepEndBy pairParser newline
  _ <- label "break" space1
  pageLists <- label "page lists" $ endBy pageListParser newline
  return (BookData ordering pageLists)

-- Tells us which pages must come before the given page
type PageRules = Map.HashMap PageNumber (Set.HashSet PageNumber)

buildRules :: [PageOrder] -> PageRules
buildRules = foldl addOrder Map.empty
  where
    addOrder m (a, b) = Map.insertWith Set.union b (Set.singleton a) m

canBeBefore :: PageRules -> PageNumber -> PageNumber -> Bool
canBeBefore rules a b = not $ Set.member b (Map.findWithDefault Set.empty a rules)

testOrder :: PageRules -> [PageNumber] -> Bool
testOrder _ [] = True
testOrder _ [_] = True
testOrder rules (h : t) = isOK && testOrder rules t
  where
    isOK = all (canBeBefore rules h) t

midpoint :: [PageNumber] -> Int
midpoint pages = pages !! (length pages `div` 2)

task1 :: PageRules -> [[PageNumber]] -> Int
task1 rules pageLists = sum $ midpoint <$> filter (testOrder rules) pageLists

sortPages :: PageRules -> [PageNumber] -> [PageNumber]
sortPages rules = sortBy cmp
  where
    cmp a b =
      if canBeBefore rules a b
        then LT
        else GT

task2 :: PageRules -> [[PageNumber]] -> Int
task2 rules pageLists = sum $ midpoint . sortPages rules <$> filter (not . testOrder rules) pageLists

tasks :: BookData -> IO ()
tasks (BookData ordering pageLists) = do
  printf "Found %d orderings and %d page lists\n" (length ordering) (length pageLists)
  let listLengths = length <$> pageLists
  printf "Longest list has %d items and the shortest has %d\n" (maximum listLengths) (minimum listLengths)
  printf "First List: %s:\n" $ show $ head pageLists
  printf "Last list: %s:\n" $ show $ last pageLists
  let orderRules = buildRules ordering
  printf "Task 1: %d\n" $ task1 orderRules pageLists
  printf "Task 2: %d\n" $ task2 orderRules pageLists

runDay5Tasks :: IO ()
runDay5Tasks = runDayilyTasks dataParser tasks "data/day5/input.txt"
