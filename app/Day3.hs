{-# LANGUAGE OverloadedStrings #-}

module Day3 where

import Control.Monad.State
import Data.Text hiding (last)
import Data.Text.IO as TIO
import Data.Void
import Text.Megaparsec as M hiding (State)
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

evaluateCmd :: Command -> State (Bool, Int) Int
evaluateCmd cmd = do
  (active, val) <- get
  case cmd of
    Do -> do
      put (True, val)
      return val
    Dont -> do
      put (False, val)
      return val
    Mul (a, b) -> do
      let val' =
            if active
              then val + a * b
              else val
      put (active, val')
      return val'

evaluateCmds :: [Command] -> State (Bool, Int) Int
evaluateCmds cmds = last <$> traverse evaluateCmd cmds
-- Note to self: I don't like the way I am using traverse and then 
-- fmap last, I think foldM is what I need, but couldn't get it to
-- work

task2 :: [Command] -> Int
task2 cmds = evalState (evaluateCmds cmds) (True, 0)

runDay3Tasks :: IO ()
runDay3Tasks = do
  let fileName = "data/day3/input.txt"
  fileText <- TIO.readFile fileName
  case runParser dataParser fileName fileText of
    Right (cmds, rest) -> do
      printf "Unparsed remainder: %s\n" rest
      printf "Last command: %s\n" $ show $ Prelude.last cmds
      printf "Task 1: %d\n" $ task1 cmds
      printf "Task 2: %d\n" $ task2 cmds
    Left e -> print $ errorBundlePretty e
