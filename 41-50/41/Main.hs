-- 41. Pandigital prime

module Main where

import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Maybe

main :: IO ()
main = print (fromJust (find prime ns))
  where
    ns = sortReverse pandigitals

pandigitals :: [Int]
pandigitals =
    let ds = [1..9]
        dss = take <$> ds <*> pure ds
    in map fromDigits (dss >>= permutations)

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

sortReverse :: Ord a => [a] -> [a]
sortReverse = sortBy (flip compare)

fromDigits :: Integral a => [Int] -> a
fromDigits = fromIntegral . read . map intToDigit
