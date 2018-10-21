module Main where

import Data.Char

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral

factorial :: (Integral a, Integral b) => a -> b
factorial n = product (map fromIntegral [2..n])

main :: IO ()
main = print (sum (toDigits (factorial 100)))
