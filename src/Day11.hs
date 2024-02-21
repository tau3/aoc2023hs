module Day11 (solvePt1, solvePt2) where

import Data.List (tails)

solvePt1 :: [String] -> Int
solvePt1 input = solve input 2

solvePt2 :: [String] ->Int -> Int
solvePt2 = solve

solve :: [String] -> Int -> Int
solve input expansion = sum distances
  where
    State _ emptyColumns emptyRows galaxies = parse input
    allPairs = pairs galaxies
    distances = map (\(l, r) -> distance l r emptyColumns emptyRows expansion) allPairs

parse :: [String] -> State
parse grid = go initial 0 0
  where
    initial = State grid [0 .. length grid - 1] [0 .. (length (head grid) - 1)] []
    height = length grid
    width = length $ head grid
    go :: State -> Int -> Int -> State
    go current row col
      | row == height = current
      | col == width = go current (row + 1) 0
      | otherwise = go (step current row col) row (col + 1)

step :: State -> Int -> Int -> State
step (State grid emptyColumns emptyRows galaxies) row col = State grid emptyColumns' emptyRows' galaxies'
  where
    value = (grid !! row) !! col
    emptyColumns' = if value == '#' then remove col emptyColumns else emptyColumns
    emptyRows' = if value == '#' then remove row emptyRows else emptyRows
    galaxies' = if value == '#' then (row, col) : galaxies else galaxies

remove :: (Eq a) => a -> [a] -> [a]
remove element = filter (/= element)

type EmptyRows = [Int]

type EmptyColumns = [Int]

type Grid = [String]

type Point = (Int, Int)

type Galaxies = [Point]

data State = State !Grid !EmptyColumns !EmptyRows !Galaxies

between :: Int -> Int -> Int -> Bool
between a b c = if a > b then (c > b) && (c < a) else (c > a) && (c < b)

distance :: Point -> Point -> EmptyColumns -> EmptyRows -> Int -> Int
distance (lr, lc) (rr, rc) emptyColumns emptyRows expansion = dx + dy + (expansion - 1) * (emptyColumnsCount + emptyRowsCount)
  where
    dx = abs (lc - rc)
    dy = abs (lr - rr)
    emptyColumnsCount = length $ filter (between lc rc) emptyColumns
    emptyRowsCount = length $ filter (between lr rr) emptyRows

pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | (x : ys) <- tails l, y <- ys]
