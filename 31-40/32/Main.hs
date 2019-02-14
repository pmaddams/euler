-- 32. Pandigital products

module Main where

import Control.Monad
import Data.Char
import Data.List

main :: IO ()
main = print (sum ns)
  where
    ns = nub [ps !! 2| ps <- pandigitalProducts]

pandigitalProducts :: Integral a => [[a]]
pandigitalProducts = do
    b <- [123..4987]
    let bs = toDigits b
    guard (unique bs)
    a <- [2..b-1]
    let as = toDigits a
    guard (unique (as ++ bs))
    let c = a * b
        cs = toDigits c
    guard (sort (as ++ bs ++ cs) == [1..9])
    return [a, b, c]

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral

unique :: Eq a => [a] -> Bool
unique xs = nub xs == xs
