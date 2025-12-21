module Day5 (solve) where

import Data.List (isInfixOf)

containsAnyLetterTwiceInRow :: String -> Bool
containsAnyLetterTwiceInRow (x1 : x2 : xs) = (x1 == x2) || containsAnyLetterTwiceInRow (x2 : xs)
containsAnyLetterTwiceInRow [] = False
containsAnyLetterTwiceInRow [_] = False

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
isNice xs = containsAnyLetterTwiceInRow xs && not (hasForbidden xs) && hasThreeVowels xs

solve :: [String] -> Int
solve = count isNice2

hasRepeatablePair :: String -> Bool
hasRepeatablePair [] = False
hasRepeatablePair [_] = False
hasRepeatablePair [_, _] = False
hasRepeatablePair [_, _, _] = False
hasRepeatablePair (x1 : x2 : xs) = [x1, x2] `isInfixOf` xs

hasLetterInBetween :: String -> Bool
hasLetterInBetween [] = False
hasLetterInBetween [_] = False
hasLetterInBetween [_, _] = False
hasLetterInBetween (x1 : x2 : x3 : xs) = (x1 == x3) || hasLetterInBetween (x2 : x3 : xs)

isNice2 :: String -> Bool
isNice2 xs = hasRepeatablePair xs && hasLetterInBetween xs
