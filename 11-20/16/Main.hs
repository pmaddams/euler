module Main where

import Data.Char

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral

main :: IO ()
main = print (sum (toDigits (2^1000)))
