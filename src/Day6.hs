module Day6 (solve) where

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

solve :: [String] -> Int
solve xs = product each_solved
  where
    parsed = parse xs
    each_solved = map (uncurry waysToWin) parsed
