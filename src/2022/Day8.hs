module Day8 (solvePt1, parseInput) where

import qualified Data.Map as M (Map, fromList, lookup)
import Data.Maybe (fromJust)

solvePt1 :: String -> M.Map String (String, String) -> Int
solvePt1 path graph = go "AAA" 0
  where
    go :: String -> Int -> Int
    go currentNode count =
      if currentNode == "ZZZ"
        then count
        else go currentNode' count'
      where
        count' = count + 1
        step = path !! mod count (length path)
        pick = if step == 'L' then fst else snd
        currentNode' = pick $ fromJust (M.lookup currentNode graph)

parseInput :: [String] -> (String, M.Map String (String, String))
parseInput input = (path, graph)
  where
    (path:_:graphData) = input
    graph = M.fromList $ map parseGraphLine graphData

parseGraphLine :: String -> (String, (String, String))
parseGraphLine l = (key, (left', right'))
  where
    ws = words l
    key = head ws
    left = ws !! 2
    left' = tail $ init left
    right = last ws
    right' = init right
