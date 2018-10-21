module Main where

import Data.Array
import Data.List
import Data.Ord

collatz :: Int -> [Int]
collatz n
    | n < 1  = []
    | n == 1 = [1]
    | even n = n : collatz (n `div` 2)
    | odd n  = n : collatz (3*n + 1)

lengthsUpTo :: Int -> [(Int, Int)]
lengthsUpTo n = assocs arr
  where
    arr = listArray (1, n) (1 : map l' [2..n])

    l' ix = l'' ix (collatz ix)

    l'' _ []        = 0
    l'' ix (c:cs)
        | c < ix    = arr ! c
        | otherwise = 1 + l'' ix cs

main :: IO ()
main = print (fst (maximumBy (comparing snd) (lengthsUpTo 1000000)))
