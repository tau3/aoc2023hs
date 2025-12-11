module Day11 (solvePt1, solvePt2) where

import Data.Map as M (Map, empty, insert, lookup)
import GHC.List as L (foldl')

solvePt1 :: [String] -> Int
solvePt1 xs = solve (inputToGraph xs) "you"

solvePt2 :: [String] -> Int
solvePt2 xs = solve2 (inputToGraph xs) "svr"

minus :: (Eq a) => [a] -> [a] -> [a]
minus left right = filter (`notElem` right) left

inputToGraph :: [String] -> Graph
inputToGraph = L.foldl' processLine M.empty
  where
    processLine :: Graph -> String -> Graph
    processLine graph line = M.insert from to graph where (from, to) = parseLine line
    parseLine :: String -> (String, [String])
    parseLine line = (init from, tail tokens)
      where
        tokens = words line
        from = head tokens

solve :: Graph -> String -> Int
solve graph start = go [] start 0
  where
    go :: [String] -> String -> Int -> Int
    go visited currentStart subResult
      | currentStart == "out" = 1
      | null adjacent'' = 0
      | otherwise = sum (map (\vertice -> go visited' vertice (subResult + 1)) adjacent'')
      where
        adjacent = M.lookup currentStart graph
        adjacent' = orElse adjacent []
        adjacent'' = minus adjacent' visited
        visited' = currentStart : visited

solve2 :: Graph -> String -> Int
solve2 graph start = go [] start 0
  where
    go :: [String] -> String -> Int -> Int
    go visited currentStart subResult
      | currentStart == "out" = if (&&) ("fft" `elem` visited) ("dac" `elem` visited) then 1 else 0
      | null adjacent'' = 0
      | otherwise = sum (map (\vertice -> go visited' vertice (subResult + 1)) adjacent'')
      where
        adjacent = M.lookup currentStart graph
        adjacent' = orElse adjacent []
        adjacent'' = minus adjacent' visited
        visited' = currentStart : visited

type Graph = M.Map String [String]

orElse :: Maybe a -> a -> a
orElse Nothing a = a
orElse (Just x) _ = x
