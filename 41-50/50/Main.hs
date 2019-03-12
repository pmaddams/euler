-- 50. Consecutive prime sums

module Main where

import Control.Arrow
import Data.List
import qualified Data.Map as M
import Data.Ord

main :: IO ()
main = print (best (primeSumsUpTo 1000000))

primeSumsUpTo :: Integral a => a -> [(a, Int)]
primeSumsUpTo n = loop (takeWhile (<= n) primes)
  where
    primeSumFrom ps =
        let sums = takeWhile (<= n) (scanl1 (+) ps)
            longest = dropWhile (not . prime) (reverse sums)
        in (head &&& length) longest

    loop []         = []
    loop ps@(p:ps') =
        let x@(s, l) = primeSumFrom ps
            xs = if s+p > n then [] else loop ps'
        in x : xs

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