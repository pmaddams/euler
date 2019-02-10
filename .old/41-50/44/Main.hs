module Main where

pentagons :: [Int]
pentagons = [n*(3*n-1) `div` 2 | n <- [1..]]

pentagonal :: Int -> Bool
pentagonal n =
    let r = sqrt (fromIntegral (24*n + 1))
        (ri, rf) = properFraction r
    in rf == 0 && ri `mod` 6 == 5

main :: IO ()
main = print $ head $
    [ (k-j)
    | k <- pentagons
    , j <- takeWhile (< k) pentagons
    , pentagonal (k-j)
    , pentagonal (j+k)
    ]
