module Day6 (solvePt1, solvePt2) where

waysToWin :: Int -> Int -> Int
waysToWin time distance = length beats
  where
    starts = [0 .. time]
    speeds = map (\s -> s * (time - s)) starts
    beats = filter (> distance) speeds

parse :: [String] -> [(Int, Int)]
parse ls = zip (head numbers) (numbers !! 1)
  where
    tokens = map words ls
    onlyRawNumbers = map dropHead tokens
    numbers = map (map (\raw -> read raw :: Int)) onlyRawNumbers

dropHead :: [n] -> [n]
dropHead (_ : xs) = xs
dropHead [] = error "unexpected empty list"

solvePt1 :: [String] -> Int
solvePt1 xs = product each_solved
  where
    parsed = parse xs
    each_solved = map (uncurry waysToWin) parsed

solvePt2 :: [String] -> Int
solvePt2 xs = waysToWin x y
  where
    (x, y) = parse2 xs

parse2 :: [String] -> (Int, Int)
parse2 xs = (read (numbers !! 0), read (numbers !! 1))
  where
    tokens = map words xs
    onlyRawNumbers = map dropHead tokens
    numbers = map concat onlyRawNumbers
