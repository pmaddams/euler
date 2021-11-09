-- 10. Summation of primes

module Main where

import qualified Data.Map as M

main :: IO ()
main = print (sum (takeWhile (< 2000000) primes))

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
