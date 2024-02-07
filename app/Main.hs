module Main (main) where

import Day7

input :: [String]
input = ["32T3K 765", "T55J5 684", "KK677 28", "KTJJT 220", "QQQJA 483"]

main :: IO ()
main = print $ solvePt1 $ parse input
