-- 14. Longest Collatz sequence

module Main where

import Data.Array
import Data.List
import Data.Ord

main :: IO ()
main = print (best (collatzLengthsUpTo 999999))

collatzLengthsUpTo :: Int -> [(Int, Int)]
collatzLengthsUpTo n = assocs a
  where
    a = listArray (1, n) (map f [1..n])

    f i = loop i (collatz i)

    loop _ []     = 0
    loop i (c:cs) =
        if c < i
        then a ! c
        else 1 + loop i cs

collatz :: Integral a => a -> [a]
collatz = takeUntil (== 1) . loop
  where
    loop n =
        if odd n
        then n : loop (3*n + 1)
        else n : loop (n `quot` 2)

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ []  = []
takeUntil p (x:xs')
    | p x       = [x]
    | otherwise = x : takeUntil p xs'

best :: Ord b => [(a, b)] -> a
best = fst . maximumBy (comparing snd)
