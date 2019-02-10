module Main where

import Data.Char

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral

lowestTerms :: (Int, Int) -> (Int, Int)
lowestTerms (n, d) =
    let g = gcd n d
    in (n `div` g, d `div` g)

equivalent :: (Int, Int) -> (Int, Int) -> Bool
equivalent r1 r2 = lowestTerms r1 == lowestTerms r2

cancelling :: (Int, Int) -> Bool
cancelling (n, d)
    | n2 == 0   = False
    | n1 == d1  = e' (n2, d2)
    | n1 == d2  = e' (n2, d1)
    | n2 == d1  = e' (n1, d2)
    | n2 == d2  = e' (n1, d1)
    | otherwise = False
  where
    (n1:n2:_) = toDigits n

    (d1:d2:_) = toDigits d

    e' = equivalent (n, d)

cancellingFractions :: [(Int, Int)]
cancellingFractions =
    let fractions = [(n, d) | d <- [12..99], n <- [11..d-1]]
    in filter cancelling fractions

multiplyFractions :: (Int, Int) -> (Int, Int) -> (Int, Int)
multiplyFractions (n, d) (n', d') = lowestTerms (n*n', d*d')

main :: IO ()
main = print (snd (foldr1 multiplyFractions cancellingFractions))
