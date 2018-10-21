module Main where

import Data.List
import qualified Data.Map as M
import Data.Ord

primes :: Integral a => [a]
primes = sieve [2..] M.empty
  where
    sieve (n:ns) m = case M.lookup n m of
        Just ps -> sieve ns (foldl mark (M.delete n m) ps)
        Nothing -> n : sieve ns (M.insert (n^2) [n] m)
      where
        mark m p = M.insertWith (++) (n+p) [p] m

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

consecutivePrimesFrom :: [Int] -> (Int, Int)
consecutivePrimesFrom [] = (0, 0)
consecutivePrimesFrom ps =
    let ss = takeWhile (< 1000000) (scanl1 (+) ps)
        cs = dropWhile (not . prime) (reverse ss)
    in (head cs, length cs)

consecutivePrimes :: [Int] -> [(Int, Int)]
consecutivePrimes []     = []
consecutivePrimes ps@(_:ps') =
    consecutivePrimesFrom ps : consecutivePrimes ps'

main :: IO ()
main =
    let ps = takeWhile (< 4000) primes
    in print (fst (maximumBy (comparing snd) (consecutivePrimes ps)))
