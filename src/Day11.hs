module Day11 (solvePt1, solvePt2) where

import Data.HashMap.Strict as M (HashMap, empty, insert, lookup, member, union, (!))
import GHC.List as L (foldl')

solvePt1 :: [String] -> Int
solvePt1 xs = solve (inputToGraph xs) "you"

solvePt2 :: [String] -> Int
solvePt2 xs = solve2 (inputToGraph xs)

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

solve2 :: Graph -> Int
solve2 graph = fst $ go "svr" [] M.empty
  where
    go :: String -> [String] -> Memo -> (Int, Memo)
    go current path memo
      | M.member current memo =
          let cacheHit = memo ! current
           in (cacheHit, memo)
      | current == "out" =
          let isValidPath = (&&) ("fft" `elem` path) ("dac" `elem` path)
           in if isValidPath then (1, memo) else (0, memo)
      | not $ M.member current graph = (0, M.insert (last path) 0 memo)
      | M.lookup current graph == Just [] = (0, M.insert (last path) 0 memo)
      | otherwise =
          let adjacent = graph ! current
              adjacent' = minus adjacent path
              (result, memo') =
                foldl'
                  ( \(accResult, accMemo) vertice ->
                      let (accResult', accMemo') = go vertice (path ++ [current]) accMemo
                       in (accResult' + accResult, M.union accMemo accMemo')
                  )
                  (0, memo)
                  adjacent'
              memo'' = M.insert (last path) result memo'
           in (result, memo'')

type Graph = M.HashMap String [String]

type Memo = M.HashMap String Int

orElse :: Maybe a -> a -> a
orElse Nothing a = a
orElse (Just x) _ = x
