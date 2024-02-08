module Main (main) where

import Day8
import System.Directory (getCurrentDirectory)
import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)

main = do
  pwd <- getCurrentDirectory
  print pwd
  handle <- openFile "../resources/day8_input" ReadMode
  contents <- hGetContents handle
  let input = lines contents
  let (path, graph) = parseInput input
  print $ solvePt1 path graph
  hClose handle
  return ()
