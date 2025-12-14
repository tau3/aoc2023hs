module Main (main) where

import Day12
import System.Directory (getCurrentDirectory)
import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)

main :: IO ()
main = do
  pwd <- getCurrentDirectory
  print pwd
  handle <- openFile "resources/day12_input" ReadMode
  contents <- hGetContents handle
  let input = lines contents
  print $ solve input
  hClose handle
  return ()
