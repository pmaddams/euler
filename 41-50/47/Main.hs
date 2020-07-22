-- 47. Distinct primes factors

module Main where

import Data.Function
import Data.List
import qualified Data.Map as M

main :: IO ()
main = print $
    let ns = [1..]
        gs = groupBy ((==) `on` snd) (zip ns (map f ns))
    in head [n | (n, m) <- map head (filter p gs), m == 4]
  where
    f = length . nub . factors
    p = (>= 4) . length

factors :: Integral a => a -> [a]
factors = loop primes
  where
    loop ps@(p:ps') n
        | n < 2           = []
        | n < p^2         = [n]
        | n `divisible` p = p : loop ps (n `quot` p)
        | otherwise       = loop ps' n

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
