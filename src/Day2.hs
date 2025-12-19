{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day2 (solve) where

import Data.List (sort)
import Data.List.Split (splitOn)

solvePt1 :: String -> Int
solvePt1 line =
  let dims = splitOn "x" line
      [l, w, h] = map read dims :: [Int]
      area = (2 * l * w) + (2 * w * h) + (2 * h * l)
      extra = minimum [l * w, w * h, h * l]
   in area + extra

solvePt2 :: String -> Int
solvePt2 line =
  let dims = splitOn "x" line
      dims'@[l, w, h] = map read dims :: [Int]
      bow = l * w * h
      [x, y, _] = sort dims'
      wrap = 2 * (x + y)
   in bow + wrap

solve :: [String] -> Int
solve xs = sum $ map solvePt2 xs
