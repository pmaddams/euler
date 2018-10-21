module Main where

import Data.Char
import Data.List

fibonacci :: [Integer]
fibonacci = 1 : 1 : zipWith (+) fibonacci (tail fibonacci)
  
toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral

main :: IO ()
main = case (findIndex ((>=1000) . length . toDigits)) fibonacci of
    Just i -> print (i+1)
