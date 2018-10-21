module Main where

import Data.List

powers :: [Integer] -> [Integer] -> [Integer]
powers as bs = nub . sort $
    [a^b | a <- as, b <- bs]

main :: IO ()
main =
    let ns = [2..100]
    in print (length (powers ns ns))
