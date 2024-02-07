module Main (main) where

import Day7
import System.Directory (getCurrentDirectory)
import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)

main :: IO ()
main = do
  pwd <- getCurrentDirectory
  print pwd
  handle <- openFile "../resources/day7_input" ReadMode
  contents <- hGetContents handle
  let input = lines contents
  print $ solvePt1 $ parse input
  hClose handle
  return ()
