-- 27. Quadratic primes

module Main where

import Control.Monad
import Data.List
import qualified Data.Map as M
import Data.Ord

main :: IO ()
main = print $
    let ps = do
            b <- takeWhile (< 1000) (dropWhile (< 40) primes)
            a <- [2-b..min 1000 (b-40)]
            guard (prime (a+b+1))
            return (a*b, length (quadraticPrimes a b))
    in (best ps)

quadraticPrimes :: Integral a => a -> a -> [a]
quadraticPrimes a b = takeWhile prime [n^2 + a*n + b | n <- [0..]]

prime :: Integral a => a -> Bool
prime = (== 1) . length . take 2 . factors

factors :: Integral a => a -> [a]
factors = loop primes
  where
    loop ps@(p:ps') n
        | n < 2           = []
        | n < p^2         = [n]
        | n `divisible` p = p : loop ps (n `quot` p)
        | otherwise       = loop ps' n

-- Adapted from M. E. O'Neill, "The Genuine Sieve of Eratosthenes"
primes :: Integral a => [a]
primes = loop [2..] M.empty
  where
    loop (n:ns) m = case (M.lookup n m) of
        Nothing -> let m' = M.insert (n^2) [n] m
                   in n : loop ns m'
        Just ps -> let f p = M.insertWith (++) (n+p) [p]
                       m' = foldr f (M.delete n m) ps
                   in loop ns m'

divisible :: Integral a => a -> a -> Bool
n `divisible` d = n `rem` d == 0

best :: Ord b => [(a, b)] -> a
best = fst . maximumBy (comparing snd)
