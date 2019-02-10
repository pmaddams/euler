module Main where

leastMultiple :: [Int] -> Int
leastMultiple = foldr1 lcm

main :: IO ()
main = print (leastMultiple [1..20])
