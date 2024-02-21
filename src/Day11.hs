module Day11 (solvePt1, parse) where

import Data.List (tails)
import Debug.Trace

solvePt1 :: [String] -> Int
solvePt1 input = sum distances
  where
    (grid, emptyColumns, emptyRows, galaxies) = parse input
    allPairs = pairs galaxies
    distances = map (\(l, r) -> distance l r emptyColumns emptyRows) allPairs

parse :: [String] -> State
parse grid = asd initial
  where
    initial = (grid, [0 .. length grid - 1], [0 .. ((length $ grid !! 0) - 1)], [])

asd :: State -> State
asd state@(grid, _, _, _) = go state 0 0
  where
    height = length grid
    width = length $ head grid
    go :: State -> Int -> Int -> State
    go current row col
      | (row == height) = current
      | col == width = go current (row + 1) 0
      | otherwise = go (step current row col) row (col + 1)

-- \| otherwise = trace ("w=" ++ show width ++ ", " ++ show height) go (step current row col) row (col + 1)

at :: [[a]] -> Int -> Int -> a
-- at xs row col = trace ("col=" ++ show col ++ "; row=" ++ show row) (xs !! row) !! col
at xs row col = (xs !! row) !! col

step :: State -> Int -> Int -> State
step (grid, emptyColumns, emptyRows, galaxies) row col = (grid, emptyColumns', emptyRows', galaxies')
  where
    -- value = (grid !! row) !! col
    value = at grid row col
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
distance l@(lr, lc) r@(rr, rc) emptyColumns emptyRows = trace ("dist " ++ show l ++ " and " ++ show r ++ " = " ++ show res) res
  where
    dx = abs (lc - rc)
    dy = abs (lr - rr)
    emptyColumnsCount = length $ filter (between lc rc) emptyColumns
    emptyRowsCount = length $ filter (between lr rr) emptyRows
    res = dx + dy + emptyColumnsCount + emptyRowsCount

pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | (x : ys) <- tails l, y <- ys]
