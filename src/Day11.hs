module Day11 (solvePt1, parse) where

import Data.List (tails)

solvePt1 :: [String] -> Int
solvePt1 input = sum distances
  where
    (grid, emptyColumns, emptyRows, galaxies) = parse input
    allPairs = pairs galaxies
    distances = map (\(l, r) -> distance l r emptyColumns emptyRows) allPairs

parse :: [String] -> State
parse grid = asd initial
  where
    initial = (grid, [0 .. length grid - 1], [0 .. (length (head grid) - 1)], [])

asd :: State -> State
asd state@(grid, _, _, _) = go state 0 0
  where
    height = length grid
    width = length $ head grid
    go :: State -> Int -> Int -> State
    go current row col
      | row == height = current
      | col == width = go current (row + 1) 0
      | otherwise = go (step current row col) row (col + 1)

step :: State -> Int -> Int -> State
step (grid, emptyColumns, emptyRows, galaxies) row col = (grid, emptyColumns', emptyRows', galaxies')
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

type State = (Grid, EmptyColumns, EmptyRows, Galaxies)

between :: Int -> Int -> Int -> Bool
between a b c = if a > b then (c > b) && (c < a) else (c > a) && (c < b)

distance :: Point -> Point -> EmptyColumns -> EmptyRows -> Int
distance (lr, lc) (rr, rc) emptyColumns emptyRows = dx + dy + emptyColumnsCount + emptyRowsCount
  where
    dx = abs (lc - rc)
    dy = abs (lr - rr)
    emptyColumnsCount = length $ filter (between lc rc) emptyColumns
    emptyRowsCount = length $ filter (between lr rr) emptyRows

pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | (x : ys) <- tails l, y <- ys]
