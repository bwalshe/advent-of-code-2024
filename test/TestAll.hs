{-# LANGUAGE OverloadedStrings #-}
module Main where

import Day6
import Data.Either as E 
import Test.HUnit
import Data.HashSet as Set

testFindLoop :: Test
testFindLoop = 
   let map = World (10,10) $ Set.fromList [(6,9),(1,6),(2,3),(7,4),(4,0),(0,8),(8,7),(9,1)]
       guard = Guard (4, 6) North
       p = getPath $ fromRight emptyPathTracker $ findPath' map guard
   in TestCase $ assertEqual "Example from website" 6 (countLoops map emptyPathTracker 0 p) 


--tests ::TestList 
--tests = [TestCase testFindLoop]


main :: IO Counts 
main = do runTestTT testFindLoop
