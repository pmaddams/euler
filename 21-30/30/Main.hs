-- 30. Digit fifth powers

module Main where

import Data.Char

main :: IO ()
main = print (sum ns)
  where
    ns = filter (sumOfDigitPowers 5) [2..1000000]

sumOfDigitPowers :: Int -> Int -> Bool
sumOfDigitPowers n i = sum (map (^n) (toDigits i)) == i

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral
