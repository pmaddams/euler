-- 29. Distinct powers

module Main where

import Data.List

main :: IO ()
main = print $
    let ns = [2..100]
    in length (nub (powers ns ns))

powers :: Integral a => [a] -> [a] -> [a]
powers as bs = [a^b | a <- as, b <- bs]
