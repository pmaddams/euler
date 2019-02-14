-- 20. Factorial digit sum

module Main where

import Data.Char

main :: IO ()
main = print (sum (toDigits n))
  where
    n = factorial 100

factorial :: Integral a => a -> a
factorial n = product [2..n]

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral
