module Main where

import Data.List
import qualified Data.Map as M

primes :: Integral a => [a]
primes = sieve [2..] M.empty
  where
    sieve (n:ns) m = case M.lookup n m of
        Just ps -> sieve ns (foldl mark (M.delete n m) ps)
        Nothing -> n : sieve ns (M.insert (n^2) [n] m)
      where
        mark m p = M.insertWith (++) (n+p) [p] m

factors :: Integral a => a -> [a]
factors n = f' n primes
  where
    f' n (p:ps)
        | n <= 1         = []
        | p^2 > n        = [n]
        | n `mod` p == 0 = p : f' (n `div` p) (p:ps)
        | otherwise      = f' n ps

runs :: Eq a => [a] -> [(a, Int)]
runs = map (\xs -> (head xs, length xs)) . group

divisors :: Integral a => a -> [a]
divisors n = sort $ d' (runs (factors n)) [1]
  where
    d' [] acc            = acc
    d' ((p, len):rs) acc = d' rs acc'
      where
        acc' = acc ++ [d * p^x | x <- [1..len], d <- acc]

sumPropDiv :: Integral a => a -> a
sumPropDiv = sum . init . divisors

amicables :: Integral a => [a]
amicables = a' [2..]
  where
    a' (n:ns) =
        let n' = sumPropDiv n
        in if n /= n' && sumPropDiv n' == n
           then n : a' ns
           else a' ns

main :: IO ()
main = print (sum (takeWhile (< 10000) amicables))
