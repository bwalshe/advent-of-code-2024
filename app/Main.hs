module Main where


import Day1 (runDay1Tasks)
import Day2 (runDay2Tasks)
import Day3 (runDay3Tasks)
import Day4 (runDay4Tasks)

import Day5 (runDay5Tasks)
tasks :: [IO ()]
tasks = [
         runDay1Tasks, 
         runDay2Tasks, 
         runDay3Tasks,
         runDay4Tasks,
         runDay5Tasks
         ]

main :: IO ()
main = last tasks 
