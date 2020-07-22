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

    f i = loop (collatz i)
      where
        loop []     = 0
        loop (c:cs) =
            if c < i
            then a ! c
            else 1 + loop cs

collatz :: Integral a => a -> [a]
collatz = takeUntil (== 1) . loop
  where
    loop n = n :
        if odd n
        then loop (3*n + 1)
        else loop (n `quot` 2)

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p xs =
    let (begin, end) = break p xs
    in begin ++ if null end then [] else [head end]

best :: Ord b => [(a, b)] -> a
best = fst . maximumBy (comparing snd)
