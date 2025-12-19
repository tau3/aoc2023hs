module Day10 (solvePt1, solvePt2) where

import Data.Set as Set (Set, empty, insert, member)

unquote :: String -> String
unquote = init . tail

parseTarget :: String -> Indicator
parseTarget = map (/= '.') . unquote

parseButton :: String -> Button
parseButton raw = map read tokens
  where
    tokens = split (unquote raw) ','

parseJoltage :: String -> Button
parseJoltage = parseButton

parseCase :: String -> (Indicator, [Button], Joltage)
parseCase input = (target, buttons, joltage)
  where
    (w : ws) = words input
    target = parseTarget w
    joltage = parseJoltage (last ws)
    buttons = map parseButton (init ws)

solvePt1Line :: String -> Int
solvePt1Line line = solvePt1' target buttons
  where
    (target, buttons, _) = parseCase line

solvePt2Line :: String -> Int
solvePt2Line line = solvePt2' joltage buttons
  where
    (_, buttons, joltage) = parseCase line

solvePt1 :: [String] -> Int
solvePt1 input = sum $ map solvePt1Line input

solvePt2 :: [String] -> Int
solvePt2 input = sum $ map solvePt2Line input

type Indicator = [Bool]

type Button = [Int]

type Joltage = [Int]

type Queue a = [(a, Int)]

solvePt1' :: Indicator -> [Button] -> Int
solvePt1' target buttons = takeRes $ trv [(initial, 0)] Set.empty
  where
    initial = map (const False) target :: Indicator
    trv :: Queue Indicator -> Set Indicator -> (Int, Queue Indicator, Set Indicator)
    trv [] _ = error "unreachable"
    trv ((st, n) : xs) seen
      | member st seen = trv xs seen
      | st == target = (n, xs, seen)
      | otherwise = trv (xs ++ moreStates) (Set.insert st seen)
      where
        moreStates = map (\button -> (pressButton st button, n + 1)) buttons
    takeRes (x, _, _) = x

solvePt2' :: Joltage -> [Button] -> Int
solvePt2' joltage buttons = takeRes $ trv [(initial, 0)] Set.empty
  where
    initial = map (const 0) joltage :: Joltage
    trv :: Queue Joltage -> Set Joltage -> (Int, Queue Joltage, Set Joltage)
    trv [] _ = error "unreachable"
    trv ((st, n) : xs) seen
      | member st seen = trv xs seen
      | st == joltage = (n, xs, seen)
      | otherwise = trv (xs ++ moreStates) (Set.insert st seen)
      where
        moreStates = map (\button -> (pressButton2 st button, n + 1)) buttons
    takeRes (x, _, _) = x

pressButton2 :: Joltage -> Button -> Joltage
pressButton2 joltage button = map (\(i, v) -> if i `elem` button then v + 1 else v) enumerated
  where
    enumerated = zip [0 ..] joltage :: [(Int, Int)]

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
