module Main (main) where

import Day6
import Day6 (solvePt1)

input :: [String]
-- input = ["Time:      7  15   30", "Distance:  9  40  200"]
input = ["Time:        54     81     70     88", "Distance:   446   1292   1035   1007"]

main :: IO ()
main = print $ solvePt2 input
