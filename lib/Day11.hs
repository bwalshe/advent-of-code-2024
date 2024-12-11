{-# LANGUAGE OverloadedStrings #-}

module Day11 where 

import Data.Text as T (Text, length, drop, take)
import qualified Data.Text.Lazy as T (toStrict)
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import Data.Text.Read (decimal)
import Text.Printf
import Data.List as L
import Data.Function.Memoize

intToText :: Integral a => a -> T.Text
intToText = T.toStrict . B.toLazyText . B.decimal

textToInteger :: Text -> Integer
textToInteger t = case decimal t of
   Right (n, _) -> n 
   Left _ -> -1

trimNum :: Text -> Text 
trimNum = intToText . textToInteger 


splitStone :: Text -> [Text]
splitStone stone = 
   let mid = T.length stone `div` 2
       s1 =  T.take mid stone 
       s2 = trimNum $ T.drop mid stone
    in [s1, s2]

mulStone :: Text -> [Text]
mulStone s = [intToText $ textToInteger s * 2024]


blink :: Text -> [Text]
blink stone 
  | stone == "0" = ["1"]
  | even (T.length stone) = splitStone stone
  | otherwise = mulStone stone



runBlinks :: Int -> [Text] -> [Text]
runBlinks 0 l = l 
runBlinks c l = runBlinks (c-1) $ concatMap blink l


task1:: Int -> [Text] -> Int 
task1 c stones  = L.length $ runBlinks c stones

efficientBlink Int -> [Int]
efficientBlink 

runDay11Tasks :: IO ()
runDay11Tasks = printf "Task 1: %d\n" $ task1 75 ["0", "44", "175060", "3442", "593", "54398", "9", "8101095"]
