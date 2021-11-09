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
    in fromDigits (ps >>= toDigits)
  where
    p = not . (1487 `elem`)

primePermutations :: Integral a => [[a]]
primePermutations =
    let ps = takeWhile (< 10000) (dropWhile (< 1000) primes)
    in do c <- ps
          let cs = toDigits c
          b <- takeWhile (< c) (dropWhile (< c `quot` 2) ps)
          let bs = toDigits b
          guard (permutation bs cs)
          let a = 2*b - c
              as = toDigits a
          guard (permutation as bs && prime a)
          return [a,b,c]

prime :: Integral a => a -> Bool
prime = (== 1) . length . take 2 . factors

factors :: Integral a => a -> [a]
factors = loop primes
  where
    loop ps@(p:ps') n
        | n < 2           = []
        | n < p^2         = [n]
        | n `divisible` p = p : loop ps (n `quot` p)
        | otherwise       = loop ps' n

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

divisible :: Integral a => a -> a -> Bool
n `divisible` d = n `rem` d == 0

permutation :: Ord a => [a] -> [a] -> Bool
permutation = (==) `on` sort

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral

fromDigits :: Integral a => [Int] -> a
fromDigits = fromIntegral . read . map intToDigit
