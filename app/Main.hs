module Main where

import Day1 (runDay1Tasks)
import Day2 (runDay2Tasks)
import Day3 (runDay3Tasks)
import Day4 (runDay4Tasks)
import Day5 (runDay5Tasks)
import Day6 (runDay6Tasks)
import Day7 (runDay7Tasks)
import Day8 (runDay8Tasks)

tasks :: [IO ()]
tasks =
  [ runDay1Tasks,
    runDay2Tasks,
    runDay3Tasks,
    runDay4Tasks,
    runDay5Tasks,
    runDay6Tasks,
    runDay7Tasks,
    runDay8Tasks
  ]

main :: IO ()
main = last tasks
