-- 30. Digit fifth powers

module Main where

import Data.Char

main :: IO ()
main = print (sum ns)
  where
    ns = filter (isDigitPowerSum 5) [2..1000000]

isDigitPowerSum :: Int -> Int -> Bool
isDigitPowerSum n i = sum (map (^n) (toDigits i)) == i

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral
