-- 1. Multiples of 3 and 5

module Main where

main :: IO ()
main = print (sum ns)
  where
    ns = filter (`divisibleAny` [3,5]) [1..999]

divisibleAny :: Integral a => a -> [a] -> Bool
divisibleAny = any . divisible

divisible :: Integral a => a -> a -> Bool
n `divisible` d = n `rem` d == 0
