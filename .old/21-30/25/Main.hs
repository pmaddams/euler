module Main where

import Data.Char
import Data.List

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
  
toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral

main :: IO ()
main = case (findIndex ((>=1000) . length . toDigits)) fibs of
    Just i -> print (i+1)
