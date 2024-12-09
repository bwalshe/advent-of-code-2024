{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Either as E
import Data.HashSet as Set
import Day6
import Day7
import Test.HUnit
import TestDay8

testFindLoop :: Test
testFindLoop =
  let map = World (10, 10) $ Set.fromList [(6, 9), (1, 6), (2, 3), (7, 4), (4, 0), (0, 8), (8, 7), (9, 1)]
      guard = Guard (4, 6) North
      p = getPath $ fromRight emptyPathTracker $ findPath' map guard
   in TestCase $ assertEqual "Example from website" 6 (countLoops map emptyPathTracker 0 p)

testDay7CheckRow :: Test
testDay7CheckRow =
  let rows =
        [ (156, [15, 6]),
          (7290, [6, 8, 6, 15]),
          (192, [17, 8, 14]),
          (190, [10, 19]),
          (292, [11, 6, 16, 20]),
          (3267, [81, 40, 27])
        ]
   in TestCase $ mapM_ (assertBool "Should be true") (checkValid2 <$> rows)

testDay7pt2 :: Test
testDay7pt2 =
  let rows =
        [ (190, [10, 19]),
          (3267, [81, 40, 27]),
          (83, [17, 5]),
          (156, [15, 6]),
          (7290, [6, 8, 6, 15]),
          (161011, [16, 10, 13]),
          (192, [17, 8, 14]),
          (21037, [9, 7, 18, 13]),
          (292, [11, 6, 16, 20])
        ]
   in TestCase $ assertEqual "Example from website" 11387 (sumValidRows checkValid2 rows)

tests :: Test
tests =
  TestList $
    [ testFindLoop,
      testDay7CheckRow,
      testDay7pt2
    ]
      ++ testDay8

main :: IO Counts
main = do runTestTT tests
