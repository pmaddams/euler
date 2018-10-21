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

sigmaZero :: Integral a => a -> Int
sigmaZero n = product $ map ((+1) . length) (group (factors n))

triangles :: [Int]
triangles = scanl1 (+) [1..]

main :: IO ()
main = case (find ((> 500) . sigmaZero) triangles) of
    Just n -> print n
