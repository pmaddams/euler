-- 40. Champernowne's constant

module Main where

import Data.Char

main :: IO ()
main = print (product (map (champernowne !!) is))
  where
    is = take 7 (map (subtract 1) (iterate (10*) 1))

champernowne :: [Int]
champernowne = [1..] >>= toDigits

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral
