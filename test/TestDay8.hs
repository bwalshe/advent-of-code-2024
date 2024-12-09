{-# LANGUAGE OverloadedStrings #-}

module TestDay8 where

import Data.HashSet as Set
import Day8
import Test.HUnit

testFindAntinodes :: Test
testFindAntinodes =
  let coords = [[(4, 3), (5, 5), (8, 4)]]
   in TestCase $
        assertEqual
          "first example on website"
          (Set.fromList [(6, 7), (3, 1), (0, 2), (2, 6)])
          (Set.fromList $ findAllAntinodes (expandCoords (10, 10)) coords)

testFindAllAntinodes :: Test
testFindAllAntinodes =
  let coords = [[(1, 1)], [(6, 7), (6, 6)], [(4, 3), (5, 5), (8, 4)]]
   in TestCase $
        assertEqual
          "second example on website"
          (Set.fromList [(6, 7), (3, 1), (0, 2), (2, 6), (6, 5), (6, 8)])
          (Set.fromList $ findAllAntinodes (expandCoords (10, 10)) coords)

testOneRay :: Coord -> Coord -> [Coord] -> Test
testOneRay a b expected =
  TestCase
    ( do
        let atob = expandRay (10, 10) a b
        assertEqual
          ("one ray from" ++ show a ++ show b)
          (Set.fromList expected)
          (Set.fromList atob)
        let btoa = expandRay (10, 10) b a
        assertEqual
          ("one ray from" ++ show a ++ show b)
          (Set.fromList expected)
          (Set.fromList btoa)
    )

testOneRayA = testOneRay (0, 0) (1, 2) [(0, 0), (1, 2), (2, 4), (3, 6), (4, 8)]

testOneRayB = testOneRay (0, 0) (3, 1) [(0, 0), (3, 1), (6, 2), (9, 3)]

testOneRayC = testOneRay (1, 2) (3, 1) [(1, 2), (3, 1), (5, 0)]

testRays :: Test
testRays =
  let coords = [[(0, 0), (1, 2), (3, 1)]]
      expected = Set.fromList [(0, 0), (1, 2), (3, 1),(2, 4), (3, 6), (4, 8), (6, 2), (9, 3), (5, 0)]
   in TestCase $
        assertEqual
          "Rays extending from 3 antenna"
          expected
          (Set.fromList $ findAllAntinodes (expandRay (10, 10)) coords)

testDay8 :: [Test]
testDay8 =
  [ testFindAntinodes,
    testFindAllAntinodes,
    testOneRayA,
    testOneRayB,
    testOneRayC,
    testRays
  ]
