-- 16. Power digit sum

module Main where

import Data.Char

main :: IO ()
main = print (sum ds)
  where
    ds = toDigits (2^1000)

toDigits :: (Integral a, Show a) => a -> [Int]
toDigits = map digitToInt . show
