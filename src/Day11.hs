module Day11 (solvePt1) where

import Data.Map as M (Map, empty, insert, lookup)
import GHC.List as L (foldl')

solvePt1 :: [String] -> Int
solvePt1 xs = solve (inputToGraph xs)

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

solve :: Graph -> Int
solve graph = go [] "you" 0
  where
    go :: [String] -> String -> Int -> Int
    go visited start subResult =
      if start == "out"
        then subResult
        else sum (map (\vertice -> go visited' vertice (subResult + 1)) adjacent'')
      where
        adjacent = M.lookup start graph
        adjacent' = orElse adjacent []
        adjacent'' = minus adjacent' visited
        visited' = start : visited

type Graph = M.Map String [String]

orElse :: Maybe a -> a -> a
orElse Nothing a = a
orElse (Just x) _ = x
