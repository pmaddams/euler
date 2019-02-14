-- 30. Digit fifth powers

module Main where

import Data.Char

main :: IO ()
main = print (sum ns)
  where
    ns = filter (sumOfPowers 5) [2..1000000]

sumOfPowers :: Integral a => a -> a -> Bool
sumOfPowers n a =
    let i = fromIntegral a
    in sum (f i) == i
  where
    f = map (^n) . toDigits

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral
