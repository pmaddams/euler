module Main where

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

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

abundant :: Integral a => a -> Bool
abundant n = sumPropDiv n > n

nonAbundantSums :: Integral a => [a]
nonAbundantSums = filter (not . abundantSum) ns
  where
    ns = [1..28123]

    as = filter abundant ns

    sas = S.fromList as

    abundantSum n = any (flip S.member sas) $
        map (n-) (takeWhile (<= n `div` 2) as)

main :: IO ()
main = print (sum nonAbundantSums)
