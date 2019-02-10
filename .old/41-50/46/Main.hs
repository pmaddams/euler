module Main where

modExp :: Integer -> Integer -> Integer -> Integer
modExp _ _ 1 = 0
modExp b x m = m' (b `mod` m) x
  where
    m' _ 0 = 1
    m' b x =
        let b' = (b^2) `mod` m
            x' = x `div` 2
        in if x `mod` 2 == 1
           then (b * m' b' x') `mod` m
           else m' b' x'

millerRabin :: Integer -> [Integer] -> Bool
millerRabin n as = all (\a -> modExp a (n-1) n == 1) as

prime :: Integral a => a -> Bool
prime n = n > 1 &&
    let ps = [2,3,5,7]
    in n `elem` ps ||
       all (\p -> n `mod` p /= 0) ps &&
       millerRabin (toInteger n) (map toInteger ps)

sumOfPrimeAndDoubleSquare :: Int -> Bool
sumOfPrimeAndDoubleSquare n =
    let doubleSquares = takeWhile (< n) (map ((*2) . (^2)) [1..])
    in s' n doubleSquares
  where
    s' _ []     = False
    s' n (d:ds) =
       if prime (n-d)
       then True
       else s' n ds

main :: IO ()
main = print $ head $
    [ n
    | n <- [3,5..]
    , not (prime n)
    , not (sumOfPrimeAndDoubleSquare n)
    ]
