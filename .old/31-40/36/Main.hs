module Main where

import Data.Char

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral

toBits :: Integral a => a -> [Int]
toBits n = b' n []
  where
    b' 0 acc = acc
    b' n acc = b' (n `div` 2) (fromIntegral (n `mod` 2) : acc)

palindrome :: Eq a => [a] -> Bool
palindrome xs = xs == reverse xs

doublePalindrome :: Integral a => a -> Bool
doublePalindrome n = odd n &&
    palindrome (toDigits n) &&
    palindrome (toBits n)

main :: IO ()
main = print (sum (filter doublePalindrome [1..999999]))
