{-# LANGUAGE OverloadedStrings #-}

module Day3 where

import Data.Text hiding (foldl)
import Text.Megaparsec as M hiding (State)
import Text.Megaparsec.Char as MC
import Text.Megaparsec.Char.Lexer as L
import Text.Printf
import Util

shortNum :: Parser Int
shortNum = do
  x <- L.decimal
  if x < 1000
    then return x
    else fail "number too large"

data Command = Mul (Int, Int) | Do | Dont deriving (Show)

mulP :: Parser Command
mulP = do
  _ <- string "mul("
  a <- shortNum
  _ <- char ','
  b <- shortNum
  _ <- char ')'
  return $ Mul (a, b)

doP :: Parser Command
doP = Do <$ string "do()"

dontP :: Parser Command
dontP = Dont <$ string "don't()"

actionP :: Parser Command
actionP = mulP <|> doP <|> dontP

dataParser :: Parser ([Command], Text)
dataParser = do
  pairs <- many (try (skipManyTill anySingle $ try actionP))
  r <- takeRest
  return (pairs, r)

task1 :: [Command] -> Int
task1 = sum . fmap act
  where
    act cmd = case cmd of
      Mul (a, b) -> a * b
      _ -> 0

evaluateCmds :: [Command] -> Int
evaluateCmds = snd . foldl updateState (True, 0)
  where
    updateState (active, val) cmd = case cmd of
      Do -> (True, val)
      Dont -> (False, val)
      Mul (a, b) ->
        let val' = if active then val + a * b else val
         in (active, val')

task2 :: [Command] -> Int
task2 = evaluateCmds

tasks :: ([Command], Text) -> IO ()
tasks (cmds, rest) = do
  printf "Unparsed remainder: %s\n" rest
  printf "Last command: %s\n" $ show $ Prelude.last cmds
  printf "Task 1: %d\n" $ task1 cmds
  printf "Task 2: %d\n" $ task2 cmds

runDay3Tasks :: IO ()
runDay3Tasks = runDailyTasks dataParser tasks "data/day3/input.txt"
