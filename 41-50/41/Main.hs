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
    in map fromDigits (concatMap permutations dss)

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

sortReverse :: Ord a => [a] -> [a]
sortReverse = sortBy (flip compare)

fromDigits :: Integral a => [Int] -> a
fromDigits = fromIntegral . read . map intToDigit
