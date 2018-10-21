module Main where

import qualified Data.Map as M

primes :: Integral a => [a]
primes = sieve [2..] M.empty
  where
    sieve (n:ns) m = case M.lookup n m of
        Just ps -> sieve ns (foldl mark (M.delete n m) ps)
        Nothing -> n : sieve ns (M.insert (n^2) [n] m)
      where
        mark m p = M.insertWith (++) (n+p) [p] m

main :: IO ()
main = print (sum (takeWhile (< 2000000) primes))
