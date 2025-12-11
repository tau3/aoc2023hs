module Day11 (solvePt1, solvePt2) where

import Data.HashMap.Strict as M (HashMap, empty, insert, lookup, union, (!?), member)
import Debug.Trace (trace)
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

-- solve2 :: Graph -> String -> Int
-- solve2 graph start = fst $ go [] (trace "START" start) M.empty
--   where
--     go :: [String] -> String -> Memo -> (Int, Memo)
--     go path from memo
--       | from == "out" = if (&&) ("fft" `elem` path) ("dac" `elem` path) then (1, memo) else (0, memo)
--       | null adjacent'' = trace "dead end" (0, memo)
--       | otherwise = case M.lookup from (trace ("cache"++show memo) memo) of
--           Just val -> trace "hit" (val, memo)
--           Nothing ->
--             let (p,s) =
--                   foldl'
--                     ( \(i, m) vertice ->
--                         let (si, sm) = go path' vertice memo
--                          in (si + i, M.union m sm)
--                     )
--                     (0, memo)
--                     adjacent''
--              in (p, M.insert (head path) p s)

--       where
--         adjacent = M.lookup from graph
--         adjacent' = orElse adjacent []
--         adjacent'' = minus adjacent' path
--         path' = path ++ [from]

solve2 :: Graph -> String -> Int
solve2 g st = fst $ cp2 g st [] M.empty

cp2:: Graph -> String -> [String] -> Memo -> (Int, Memo)
cp2 g f p m 
  | M.member f m  = (unJust $ M.lookup f m, trace ("cache" ++ show m) m)
  | f == "out" = if (&&) ("fft" `elem` p) ("dac" `elem` p) then (1, m) else (0, m)
  | M.lookup f g == Nothing = (0, M.insert (last p) 0 m)
  | M.lookup f g == Just[] = (0, M.insert (last p) 0 m)
  | otherwise = let a = unJust $ M.lookup f g
                    a' = minus a p  
                    (r, m') = foldl' (\(sr, sm) v-> let (ssr, ssm) = cp2 g v (p ++[f]) m in (ssr+sr, M.union sm ssm)) (0, m) a'
                in (r, M.insert (last p) r m')
  

type Graph = M.HashMap String [String]

type Memo = M.HashMap String Int

unJust::Maybe a -> a
unJust (Nothing) = error "fail"
unJust (Just x) = x

orElse :: Maybe a -> a -> a
orElse Nothing a = a
orElse (Just x) _ = x
