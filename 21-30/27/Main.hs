-- 27. Quadratic primes

module Main where

import Control.Monad
import Data.List
import qualified Data.Map as M
import Data.Ord

main :: IO ()
main = (print . best) $
    do b <- takeWhile (< 1000) (dropWhile (< 40) primes)
       a <- [2-b..min 1000 (b-40)]
       guard (prime (1 + a + b))
       return (a * b, length (quadraticPrimes a b))

quadraticPrimes :: Integral a => a -> a -> [a]
quadraticPrimes a b = takeWhile prime [n^2 + a*n + b | n <- [0..]]

prime :: Integral a => a -> Bool
prime n =
    let ps = [2,3,5]
    in elem n ps ||
       not (anyDivisible n ps) &&
       length (factors n) == 1

factors :: Integral a => a -> [a]
factors n = loop n primes
  where
    loop n ps@(p:ps')
        | n < 2         = []
        | n < p^2       = [n]
        | divisible n p = p : loop (quot n p) ps
        | otherwise     = loop n ps'

primes :: Integral a => [a]
primes = loop [2..] M.empty
  where
    loop (n:ns) m = case (M.lookup n m) of
        Nothing -> let m' = M.insert (n^2) [n] m
                   in n : loop ns m'
        Just ps -> let f p = M.insertWith (++) (n+p) [p]
                       m' = foldr f (M.delete n m) ps
                   in loop ns m'

anyDivisible :: Integral a => a -> [a] -> Bool
anyDivisible = any . divisible

divisible :: Integral a => a -> a -> Bool
divisible n d = rem n d == 0

best :: Ord b => [(a, b)] -> a
best = fst . maximumBy (comparing snd)
