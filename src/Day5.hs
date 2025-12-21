module Day5 (solve) where

import Data.List (isInfixOf)

containsTwice :: String -> Bool
containsTwice (x1 : x2 : xs) = (x1 == x2) || containsTwice (x2 : xs)
containsTwice [] = False
containsTwice [_] = False

hasForbidden :: String -> Bool
hasForbidden xs = any (`isInfixOf` xs) forbidden
  where
    forbidden = ["ab", "cd", "pq", "xy"]

hasThreeVowels :: String -> Bool
hasThreeVowels xs = vowels >= 3
  where
    vowels = count (`elem` "aeiou") xs

count :: (a -> Bool) -> [a] -> Int
count _ [] = 0
count f (x : xs) = if f x then 1 + count f xs else count f xs

isNice :: String -> Bool
isNice xs = containsTwice xs && not (hasForbidden xs) && hasThreeVowels xs

solve :: [String] -> Int
solve = count isNice
