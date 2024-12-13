{-# LANGUAGE OverloadedStrings #-}

module Day13 where

import Data.Either (rights)
import Data.Matrix as Mat
import Data.Ratio (denominator, numerator)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Printf
import Util

newtype Vector = Vector (Int, Int) deriving (Show)

instance Semigroup Vector where
  (<>) (Vector (x1, y1)) (Vector (x2, y2)) = Vector (x1 + x2, y1 + y2)

instance Monoid Vector where
  mempty = Vector (0, 0)

data ButtonLable = A | B deriving (Show, Eq)

data Button = Button {getLable :: ButtonLable, getVector :: Vector} deriving (Show)

data Game = Game {getA :: Button, getB :: Button, getTarget :: Vector} deriving (Show)

signedInteger :: Parser Int
signedInteger = L.signed space (L.lexeme space L.decimal)

pLable :: Parser ButtonLable
pLable =
  choice
    [ A <$ single 'A',
      B <$ single 'B'
    ]

pButton :: Parser Button
pButton = do
  _ <- string "Button "
  l <- pLable
  _ <- string ": X"
  x <- signedInteger
  _ <- string ", Y"
  y <- signedInteger
  return $ Button l $ Vector (x, y)

pTarget :: Parser Vector
pTarget = do
  _ <- string "Prize: X="
  x <- signedInteger
  _ <- string ", Y="
  y <- signedInteger
  return $ Vector (x, y)

pGame :: Parser Game
pGame = do
  a <- pButton
  -- _ <- newline
  b <- pButton
  -- _ <- newline
  t <- pTarget
  return $ Game a b t

pData :: Parser [Game]
pData = many pGame

cost :: Button -> Int
cost (Button A _) = 3
cost (Button B _) = 1

makeBasis :: Vector -> Vector -> Either String (Matrix Rational)
makeBasis (Vector (x1, y1)) (Vector (x2, y2)) = Mat.inverse m
  where
    m = Mat.fromList 2 2 $ fromIntegral <$> [x1, x2, y1, y2]

tryInteger :: Rational -> Either String Integer
tryInteger r
  | denominator r /= 1 = Left $ "denominator is " ++ show (denominator r)
  | otherwise = Right $ numerator r

solveGame :: Game -> Either String Integer
solveGame (Game a b (Vector (x, y))) =
  case makeBasis (getVector a) (getVector b) of
    Right basis ->
      do
        let vt = Mat.fromList 2 1 $ fromIntegral <$> [x, y]
        let transformed = Mat.multStd basis vt
        let costs = Mat.fromList 1 2 $ fromIntegral <$> [cost a, cost b]
        tryInteger <$> Mat.getElem 1 1 $ Mat.multStd costs transformed
    Left e -> Left e

task1 :: [Game] -> Integer
task1 games = sum $ rights $ solveGame <$> games

nudge :: Game -> Game
nudge (Game a b (Vector (x, y))) = Game a b $ Vector (10000000000000 + x, 10000000000000 + y)

task2 :: [Game] -> Integer
task2 games = sum $ rights $ solveGame . nudge <$> games

tasks :: [Game] -> IO ()
tasks games = do
  printf "Task 1 %d\n" $ task1 games
  printf "Task 2: %d\n" $ task2 games

runDay13Tasks :: IO ()
runDay13Tasks = runDailyTasks pData tasks "data/day13/input.txt" -- "/tmp/test.txt"
