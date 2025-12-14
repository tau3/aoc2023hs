module Day12 (solve) where

import Data.List.Split (splitOn)
import GHC.List (foldl')

solve :: [String] -> Int
solve input =
  let cases = reverse input
      cases' = takeWhile (/= "") cases
      parsedCases = map parseInputLine cases'
      solutions = map solveOne parsedCases
   in foldl' (\acc sol -> if sol then acc + 1 else acc) 0 solutions

parseInputLine :: String -> (Int, [Int])
parseInputLine line =
  let (region : quantities) = words line
      quantities' = map read quantities
      area = calcArea region
   in (area, quantities')

calcArea :: String -> Int
calcArea input =
  let tokens = splitOn "x" input
   in case tokens of
        [w, h] -> read w * read (init h)
        _ -> error $ "invalid input " ++ input

solveOne :: (Int, [Int]) -> Bool
solveOne (area, quantities) =
  let minRequiredArea = sum quantities * 3 * 3
   in area >= minRequiredArea
