module Day11 (solvePt1, solvePt2) where

import Data.HashMap.Strict as M (HashMap, empty, insert, lookup, union)
import Data.Set as S (Set, empty, insert)
import GHC.List as L (foldl')

solvePt1 :: [String] -> Int
solvePt1 xs = solve (inputToGraph xs) "you"

solvePt2 :: [String] -> Int
solvePt2 xs = solve2 (inputToGraph xs) "svr"

minus :: (Eq a) => [a] -> S.Set a -> [a]
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
solve graph start = go S.empty start 0
  where
    go :: S.Set String -> String -> Int -> Int
    go visited currentStart subResult
      | currentStart == "out" = 1
      | null adjacent'' = 0
      | otherwise = sum (map (\vertice -> go visited' vertice (subResult + 1)) adjacent'')
      where
        adjacent = M.lookup currentStart graph
        adjacent' = orElse adjacent []
        adjacent'' = minus adjacent' visited
        visited' = S.insert currentStart visited

solve2 :: Graph -> String -> Int
solve2 graph start = fst $ go start S.empty start 0 M.empty
  where
    go :: String -> S.Set String -> String -> Int -> Memo -> (Int, Memo)
    go begin visited currentStart subResult memo
      | currentStart == "out" = if (&&) ("fft" `elem` visited) ("dac" `elem` visited) then (1, M.insert begin 1 memo) else (0, M.insert begin 0 memo)
      | null adjacent'' = (0, M.insert begin 0 memo)
      | otherwise =
          let y = map (\vertice -> go currentStart visited' vertice (subResult + 1) memo) adjacent''
           in L.foldl' (\(accSubRes, accMemo) (curSubres, curMemo) -> (accSubRes + curSubres, M.union accMemo curMemo)) (0, M.empty) y
      where
        adjacent = M.lookup currentStart graph
        adjacent' = orElse adjacent []
        adjacent'' = minus adjacent' visited
        visited' = S.insert currentStart visited

type Graph = M.HashMap String [String]

type Memo = M.HashMap String Int

orElse :: Maybe a -> a -> a
orElse Nothing a = a
orElse (Just x) _ = x
