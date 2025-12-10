module Day6 (solvePt1, solvePt2) where

waysToWin :: Int -> Int -> Int
waysToWin time distance = length beats
  where
    starts = [0 .. time]
    speeds = map (\speed -> speed * (time - speed)) starts
    beats = filter (> distance) speeds

parsePt1 :: [String] -> [(Int, Int)]
parsePt1 input = zip (head numbers) (numbers !! 1)
  where
    tokens = map words input
    onlyRawNumbers = map dropHead tokens
    numbers = map (map (\raw -> read raw :: Int)) onlyRawNumbers

dropHead :: [n] -> [n]
dropHead (_ : xs) = xs
dropHead [] = error "unexpected empty list"

solvePt1 :: [String] -> Int
solvePt1 xs = product each_solved
  where
    parsed = parsePt1 xs
    each_solved = map (uncurry waysToWin) parsed

solvePt2 :: [String] -> Int
solvePt2 input = waysToWin time distance
  where
    (time, distance) = parsePt2 input

parsePt2 :: [String] -> (Int, Int)
parsePt2 input = (read $ head numbers, read $ numbers !! 1)
  where
    tokens = map words input
    onlyRawNumbers = map dropHead tokens
    numbers = map concat onlyRawNumbers
