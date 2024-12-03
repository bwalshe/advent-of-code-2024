{-# LANGUAGE OverloadedStrings #-}

module Day3 where

import Data.Text
import Data.Text.IO as TIO
import Data.Void
import Text.Megaparsec as M
import Text.Megaparsec.Char as MC
import Text.Megaparsec.Char.Lexer as L
import Text.Printf

type Parser = Parsec Void Text

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

task2 :: [Command] -> Int
task2 = second . Prelude.foldl act (True, 0)
  where
    act state cmd = case (state, cmd) of
      ((True, val), Mul (a, b)) -> (True, val + a * b)
      ((_, val), Do) -> (True, val)
      ((_, val), Dont) -> (False, val)
      _ -> state
    second (_, b) = b

runDay3Tasks :: IO ()
runDay3Tasks = do
  let fileName = "data/day3/input.txt"
  fileText <- TIO.readFile fileName
  case runParser dataParser fileName fileText of
    Right (cmds, rest) -> do
      printf "remainder: %s\n" rest
      printf "Last command: %s\n" $ show $ Prelude.last cmds
      printf "Task 1: %d\n" $ task1 cmds
      printf "Task 2: %d\n" $ task2 cmds
    Left e -> print $ errorBundlePretty e
