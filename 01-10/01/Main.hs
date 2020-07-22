-- 1. Multiples of 3 and 5

module Main where

main :: IO ()
main = print (sum ns)
  where
    ns = filter (`anyDivisible` [3, 5]) [1..999]

anyDivisible :: Integral a => a -> [a] -> Bool
anyDivisible = any . divisible

divisible :: Integral a => a -> a -> Bool
n `divisible` d = n `rem` d == 0
