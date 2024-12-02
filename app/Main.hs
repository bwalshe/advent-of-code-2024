module Main where


import Day1 (runDay1Tasks)
import Day2 (runDay2Tasks)

tasks :: [IO ()]
tasks = [runDay1Tasks, runDay2Tasks]

main :: IO ()
main = last tasks 
