module Main where

import Data.Char

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral

factorial :: (Integral a, Integral b) => a -> b
factorial n = product (map fromIntegral [2..n])

sumDigitFactorials :: Int -> Int
sumDigitFactorials = sum . (map factorial) . toDigits

main :: IO ()
main = print (sum [n | n <- [3..50000], n == sumDigitFactorials n])
