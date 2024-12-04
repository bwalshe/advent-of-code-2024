{-# LANGUAGE OverloadedStrings #-}

module Day4 where

import Data.Maybe (fromMaybe)
import Data.Text as T hiding (elem, take, zip)
import Data.Text.IO as TIO
import Text.Printf

xmas :: Text
xmas = "XMAS"

countXmas :: Text -> Int
countXmas = countXmas' 0
  where
    countXmas' n l
      | xmas `isPrefixOf` l = countXmas' (n + 1) (skip l)
      | T.length l > 0 = countXmas' n (T.tail l)
      | otherwise = n
    skip l = fromMaybe "" $ stripPrefix xmas l

diagonal :: [Text] -> [Text]
diagonal rows = diagonal' rows ++ Prelude.tail (diagonal' $ transpose rows)
  where
    diagonal' r = transpose $ trim <$> zip [0 ..] r
    trim (i, l) = T.drop i l

reverseHorizontal :: [Text] -> [Text]
reverseHorizontal = fmap T.reverse

reverseVertical :: [Text] -> [Text]
reverseVertical = Prelude.reverse

allDirections :: [Text] -> [Text]
allDirections rows = Prelude.concatMap (\op -> op rows) ops
  where
    ops =
      [ id,
        transpose,
        reverseHorizontal,
        transpose . reverseVertical,
        diagonal,
        diagonal . reverseHorizontal,
        diagonal . reverseHorizontal . reverseVertical,
        diagonal . reverseVertical
      ]

countAllXmas :: [Text] -> Int
countAllXmas rows = sum $ countXmas <$> allDirections rows

validPatterns :: [(Text, Text, Text)]
validPatterns = makeTuple <$> ((\op -> op ["M.M", ".A.", "S.S"]) <$> ops1) ++ ((\op -> op [".M.", "MAS", ".S."]) <$> ops2)
  where
    ops1 =
      [ id,
        transpose,
        reverseVertical,
        reverseHorizontal . transpose
      ]
    ops2 =
      [ id,
        reverseVertical,
        reverseHorizontal,
        reverseVertical . reverseHorizontal
      ]
    makeTuple [a, b, c] = (a, b, c)
    makeTuple _ = undefined

mask :: [Bool] -> Text -> Text
mask pattern t = pack $ f <$> zip pattern (unpack t)
  where
    f (True, c) = c
    f (False, _) = '.'

countXmasPattern :: [Bool] -> [Bool] -> Int -> Text -> Text -> Text -> Int
countXmasPattern p1 p2 n row1 row2 row3
  | row1 == "" || row2 == "" || row3 == "" = n
  | otherwise =
      let pattern = (mask p1 row1, mask p2 row2, mask p1 row3)
          n' =
            if pattern `elem` validPatterns
              then n + 1
              else n
          row1' = T.drop 1 row1
          row2' = T.drop 1 row2
          row3' = T.drop 1 row3
       in countXmasPattern p1 p2 n' row1' row2' row3'

countAllXmasPattern :: [Text] -> Int
countAllXmasPattern rows = countall xox oxo 0 rows + countall oxo xox 0 rows
  where
    countall p1 p2 n rows' = case take 3 rows' of
      [row1, row2, row3] -> countall p1 p2 (n + countXmasPattern p1 p2 0 row1 row2 row3) (Prelude.drop 1 rows')
      _ -> n
    xox = [True, False, True]
    oxo = [False, True, False]

runDay4Tasks :: IO ()
runDay4Tasks = do
  rows <- T.lines <$> TIO.readFile "data/day4/input.txt"
  printf "read %d lines\n" $ Prelude.length rows
  printf "row length: %d\n" $ T.length $ Prelude.head rows
  printf "Task 1: %d\n" $ countAllXmas rows
  printf "Task 2: %d\n" $ countAllXmasPattern rows
