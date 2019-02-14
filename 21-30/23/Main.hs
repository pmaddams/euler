-- 23. Non-abundant sums

module Main where

import Control.Monad
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = print (sum nonAbundantSums)

nonAbundantSums :: Integral a => [a]
nonAbundantSums = filter (not . p) ns
  where
    ns = [1..28123]

    as = filter abundant ns

    s = S.fromList as

    p n = let ms = takeWhile (<= quot n 2) as
          in any (flip S.member s) (map (n-) ms)

abundant :: Integral a => a -> Bool
abundant n = d n > n
  where
    d = sum . init . divisors

divisors :: Integral a => a -> [a]
divisors n =
    let ds = foldr f [1] (factors n)
    in sort (nub ds)
  where
    f p ds = ds ++ map (p*) ds

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

divisible :: Integral a => a -> a -> Bool
divisible n d = rem n d == 0