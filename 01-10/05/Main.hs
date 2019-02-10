-- 5. Smallest multiple

module Main where

main :: IO ()
main = print (smallestMultiple [1..20])

smallestMultiple :: Integral a => [a] -> a
smallestMultiple = foldr1 lcm
