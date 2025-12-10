module Day10 (solvePt1) where

import Data.Set as Set (Set, empty, insert, member)

unquote :: String -> String
unquote = init . tail

parseTarget :: String -> Indicator
parseTarget = map (/= '.') . unquote

parseButton :: String -> Button
parseButton raw = map read tokens
  where
    tokens = split (unquote raw) ','

solvePt1Line :: String -> Int
solvePt1Line line = solvePt1' target buttons
  where
    (targetRaw : xs) = words line :: [String]
    target = parseTarget targetRaw
    xs' = init xs :: [String]
    buttons = map parseButton xs'

solvePt1 :: [String] -> Int
solvePt1 input = sum $ map solvePt1Line input

type Indicator = [Bool]

type Button = [Int]

type Queue = [(Indicator, Int)]

solvePt1' :: Indicator -> [Button] -> Int
solvePt1' target buttons = takeRes $ trv [(initial, 0)] Set.empty
  where
    initial = map (const False) target :: Indicator
    trv :: Queue -> Set Indicator -> (Int, Queue, Set Indicator)
    trv [] _ = error "unreachable"
    trv ((st, n) : xs) seen
      | member st seen = trv xs seen
      | st == target = (n, xs, seen)
      | otherwise = trv (xs ++ moreStates) (Set.insert st seen)
      where
        moreStates = map (\button -> (pressButton st button, n + 1)) buttons
    takeRes (x, _, _) = x

pressButton :: Indicator -> Button -> Indicator
pressButton indicator button = map (\(i, v) -> if i `elem` button then not v else v) enumerated
  where
    enumerated = zip [0 ..] indicator :: [(Int, Bool)]

split :: String -> Char -> [String]
split str sep = wordsWhen (== sep) str

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'
