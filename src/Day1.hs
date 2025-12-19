{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day1 (solve) where

solvePt1 :: String -> Int
solvePt1 = foldr (\x -> (+) (if x == '(' then 1 else -1)) 0

solve :: [String] -> Int
solve xs = solvePt2 $ head xs

solvePt2 :: String -> Int
solvePt2 = go 0 1
  where
    go :: Int -> Int -> String -> Int
    go f i (x : xs) =
      let f' = if x == '(' then f + 1 else f - 1
       in if f' < 0 then i else go f' (i + 1) xs
