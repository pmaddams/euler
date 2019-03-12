-- 46. Goldbach's other conjecture

module Main where

import Data.List
import qualified Data.Map as M
import Data.Maybe

main :: IO ()
main = print (fromJust (find (not . sumPrimeTwiceSquare) ns))
  where
    ns = [3,5..] `without` primes

sumPrimeTwiceSquare :: Integral a => a -> Bool
sumPrimeTwiceSquare n =
    let ms = takeWhile (< n) twiceSquares
    in loop n ms
  where
    loop _ []     = False
    loop n (m:ms) = prime (n-m) || loop n ms

twiceSquares :: Integral a => [a]
twiceSquares = map ((*2) . (^2)) [1..]

without :: Ord a => [a] -> [a] -> [a]
[] `without` _  = []
xs `without` [] = xs
xs@(x:xs') `without` ys@(y:ys')
    | x < y     = x : xs' `without` ys
    | x > y     = xs `without` ys'
    | otherwise = xs' `without` ys'

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
