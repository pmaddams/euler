module Main where

import Data.List

hexagons :: [Int]
hexagons = [n*(2*n-1) | n <- [1..]]

pentagonal :: Int -> Bool
pentagonal n =
    let r = sqrt (fromIntegral (24*n + 1))
        (ri, rf) = properFraction r
    in rf == 0 && ri `mod` 6 == 5

main :: IO ()
main =
    let hs = dropWhile (<= 40755) hexagons
    in case (find pentagonal hs) of
           Just n -> print n
