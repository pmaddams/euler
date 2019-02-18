-- 49. Prime permutations

module Main where

import Control.Monad
import Data.Char
import Data.Function
import Data.List
import qualified Data.Map as M

main :: IO ()
main = print $
    let ps = head (filter p primePermutations)
    in fromDigits (concatMap toDigits ps)
  where
    p = not . elem 1487

primePermutations :: Integral a => [[a]]
primePermutations =
    let ps = takeWhile (< 10000) (dropWhile (< 1000) primes)
    in do c <- ps
          let cs = toDigits c
          b <- takeWhile (< c) (dropWhile (< (quot c 2)) ps)
          let bs = toDigits b
          guard (permutation bs cs)
          let a = 2*b - c
              as = toDigits a
          guard (permutation as bs && prime a)
          return [a,b,c]

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

permutation :: Ord a => [a] -> [a] -> Bool
permutation = (==) `on` sort

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral

fromDigits :: Integral a => [Int] -> a
fromDigits = fromIntegral . read . map intToDigit
