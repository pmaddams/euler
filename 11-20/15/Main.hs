-- 15. Lattice paths

module Main where

main :: IO ()
main = print (paths 20)

paths :: Integral a => a -> a
paths n = (2*n) `choose` n

choose :: Integral a => a -> a -> a
n `choose` k = product [k+1..n] `quot` product [2..n-k]
