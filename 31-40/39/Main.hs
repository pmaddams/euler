-- 39. Integer right triangles

module Main where

import Control.Monad
import Data.List
import Data.Ord

main :: IO ()
main = print $
    let ps = do
            p <- takeWhile (<= 1000) perimeters
            p' <- takeWhile (<= 1000) (iterate (+p) p)
            return p'
    in best (counts ps)

perimeters :: Integral a => [a]
perimeters = do
    n <- [2..]
    m <- [1..n-1]
    guard (odd (n+m))
    guard (gcd n m == 1)
    let a = n^2 - m^2
        b = 2*n*m
        c = n^2 + m^2
    return (a+b+c)

counts :: Eq a => [a] -> [(a, Int)]
counts []       = []
counts xs@(y:_) = (y, length ys) : counts zs
  where
    (ys, zs) = partition (== y) xs

best :: Ord b => [(a, b)] -> a
best = fst . maximumBy (comparing snd)
