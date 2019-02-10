module Main where

choose :: Integral a => Int -> Int -> a
n `choose` k =
    let f = product . (map fromIntegral)
    in f [k+1..n] `div` f [2..n-k]

paths :: Int -> Integer
paths n = (2*n) `choose` n

main :: IO ()
main = print (paths 20)
