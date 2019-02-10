module Main where

import Data.Char

isSumOfPowers :: Int -> Int -> Bool
isSumOfPowers x n = s' (map digitToInt (show n)) 0
  where
    s' [] acc     = n == acc
    s' (d:ds) acc =
        if acc > n 
        then False
        else s' ds (d^x + acc)

main :: IO ()
main = print (sum [n | n <- [2..1000000], isSumOfPowers 5 n])
