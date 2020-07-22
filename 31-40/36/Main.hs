-- 36. Double-base palindromes

module Main where

import Data.Char

main :: IO ()
main = print (sum (filter p [1..999999]))
  where
    p n = all palindrome [toDigits n, toBits n]

palindrome :: Eq a => [a] -> Bool
palindrome xs = xs == reverse xs

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral

toBits :: Integral a => a -> [Int]
toBits 0 = []
toBits n =
    let (q, r) = n `quotRem` 2
    in fromIntegral r : toBits q
