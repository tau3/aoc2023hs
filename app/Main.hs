module Main (main) where

import Day11
import System.Directory (getCurrentDirectory)
import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)

main = do
  pwd <- getCurrentDirectory
  print pwd
  handle <- openFile "resources/day11_input" ReadMode
  contents <- hGetContents handle
  let input = lines contents
  print $ solvePt1 input
  hClose handle
  return ()
