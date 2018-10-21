module Main where

import Data.Char
import Data.List

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral

fromDigits :: Integral a => [Int] -> a
fromDigits = fromIntegral . read . concatMap show
 
pandigitalProducts :: [(Int, Int, Int)]
pandigitalProducts =
    [ (m1, m2, p)
    | ns <- permutations [1..9]
    , let (lo, hi) = splitAt 5 ns
    , i <- [1, 2]
    , let (ll, lh) = splitAt i lo
          m1 = fromDigits ll
          m2 = fromDigits lh
          p = m1 * m2
    , sort (toDigits p) == hi
    ]

main :: IO ()
main = print (sum (nub [p | (_,_,p) <- pandigitalProducts]))
