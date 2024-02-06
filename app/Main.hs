module Main (main) where

import Day6

input :: [String]
input = ["Time:      7  15   30", "Distance:  9  40  200"]

main :: IO ()
main = print $ solve input
