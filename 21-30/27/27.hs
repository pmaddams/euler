module Main where

import Data.List
import qualified Data.Map as M
import Data.Ord

check :: String -> [Bool] -> IO ()
check name tests =
    if not (and tests)
    then error name
    else return ()

primes :: Integral a => [a]
primes = sieve [2..] M.empty
  where
    sieve (n:ns) m = case M.lookup n m of
        Just ps -> sieve ns (foldl mark (M.delete n m) ps)
        Nothing -> n : sieve ns (M.insert (n^2) [n] m)
      where
        mark m p = M.insertWith (++) (n+p) [p] m

checkPrimes :: IO ()
checkPrimes = check "primes"
    [ take 6 primes == [2,3,5,7,11,13]
    , primes !! 999 == 7919
    ]

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

checkModExp :: IO ()
checkModExp = check "modExp"
    [ modExp 5 3 3 == 2
    , modExp 2 3 5 == 3
    ]

millerRabin :: Integer -> [Integer] -> Bool
millerRabin n []     = True
millerRabin n (a:as) =
    modExp a (n-1) n == 1 &&
    millerRabin n as

smallPrime :: Int -> Bool
smallPrime n = n > 1 &&
    let ps = [2,3,5,7]
    in n `elem` ps ||
       all (\p -> n `mod` p /= 0) ps &&
       millerRabin (toInteger n) (map toInteger ps)

checkSmallPrime :: IO ()
checkSmallPrime = check "smallPrime"
    [ all smallPrime [2,3,5,7,11,13]
    , not (any smallPrime [-1,0,1,4,9])
    ]

maximumAB :: (Int, Int)
maximumAB = fst $ maximumBy (comparing snd) $
    [ ((a, b), len)
    | b <- takeWhile (< 1000) (dropWhile (< 41) primes)
    , a <- [(2-b)..min (b-40) 1000]
    , smallPrime (1 + a + b)
    , let len = length $ takeWhile smallPrime
              [n^2 + a*n + b | n <- [2..1000]]
    ]

test :: IO ()
test = do
    checkPrimes
    checkModExp
    checkSmallPrime

main :: IO ()
main =
    let (a, b) = maximumAB
    in print (a*b)
