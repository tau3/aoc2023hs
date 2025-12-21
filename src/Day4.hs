module Day4 where

import Crypto.Hash
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as L
import Data.List (isPrefixOf)

stringToLazyByteString :: String -> L.ByteString
stringToLazyByteString = L.fromStrict . BS.pack

md5 :: String -> String
md5 str =
  let lbs = stringToLazyByteString str
      digest = hashlazy lbs :: Digest MD5
   in show digest

solvePt1 :: String
solvePt1 = solve 5

solvePt2 :: String
solvePt2 = solve 6

solve :: Int -> String
solve n = show $ findFirst [1 ..] check
  where
    check :: Int -> Bool
    check x =
      let num = "ckczppom" ++ show x
          val = md5 num
       in isPrefixOf (replicate n '0') val

findFirst :: [a] -> (a -> Bool) -> a
findFirst [] _ = error "fail"
findFirst (x : xs) f = if f x then x else findFirst xs f
