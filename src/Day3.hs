module Day3 (solve) where

import qualified Data.Set as Set

solve :: [String] -> Int
solve = solvePt2 . head

nextPos :: (Int, Int) -> Char -> (Int, Int)
nextPos (x, y) '^' = (x, y - 1)
nextPos (x, y) '>' = (x + 1, y)
nextPos (x, y) 'v' = (x, y + 1)
nextPos (x, y) '<' = (x - 1, y)
nextPos _ dir = error $ "unexpected direction " ++ [dir]

solvePt1 :: String -> Int
solvePt1 line = go (0, 0) line (Set.singleton (0, 0))
  where
    go :: (Int, Int) -> String -> Set.Set (Int, Int) -> Int
    go _ [] visited = Set.size visited
    go pos (direction : rest) visited =
      let pos' = nextPos pos direction
          visited' = Set.insert pos' visited
       in go pos' rest visited'

solvePt2 :: String -> Int
solvePt2 line = go (0, 0) (0, 0) True line (Set.singleton (0, 0))
  where
    go :: (Int, Int) -> (Int, Int) -> Bool -> String -> Set.Set (Int, Int) -> Int
    go _ _ _ [] visited = Set.size visited
    go santa robo True (direction : rest) visited =
      let santa' = nextPos santa direction
          visited' = Set.insert santa' visited
       in go santa' robo False rest visited'
    go santa robo False (direction : rest) visited =
      let robo' = nextPos robo direction
          visited' = Set.insert robo' visited
       in go santa robo' True rest visited'
