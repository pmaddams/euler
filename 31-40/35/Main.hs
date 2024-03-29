-- 35. Circular primes

module Main where

import Data.Char
import qualified Data.Map as M

main :: IO ()
main = print (length (filter circular ps))
  where
    ps = takeWhile (< 1000000) primes

circular :: Integral a => a -> Bool
circular p =
    let ds = toDigits p
        ns = map fromDigits (rotations ds)
    in length ds == 1 ||
       not (any (`elem` ds) [0,2,4,5,6,8]) &&
       all prime ns

rotations :: [a] -> [[a]]
rotations xs = take (length xs) (loop xs)
  where
    loop xs@(x:xs') = xs : loop (xs' ++ [x])

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

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral

fromDigits :: Integral a => [Int] -> a
fromDigits = fromIntegral . read . map intToDigit
