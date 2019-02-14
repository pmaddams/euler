-- 34. Digit factorials

module Main where

import Data.Char

main :: IO ()
main = print (sum (filter p [3..50000]))
  where
    p n = sum (map factorial (toDigits n)) == n

factorial :: Integral a => a -> a
factorial n = product [2..n]

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral
