-- 3. Largest prime factor

module Main where

import qualified Data.Map as M

main :: IO ()
main = print (last (factors 600851475143))

factors :: Integral a => a -> [a]
factors n = loop n primes
  where
    loop n ps@(p:ps')
        | n < 2           = []
        | n < p^2         = [n]
        | n `divisible` p = p : loop (n `quot` p) ps
        | otherwise       = loop n ps'

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
