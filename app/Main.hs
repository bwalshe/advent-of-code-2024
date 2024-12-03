module Main where


import Day1 (runDay1Tasks)
import Day2 (runDay2Tasks)
import Day3 (runDay3Tasks)


tasks :: [IO ()]
tasks = [runDay1Tasks, runDay2Tasks, runDay3Tasks]

main :: IO ()
main = last tasks 
