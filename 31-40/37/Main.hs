-- 37. Truncatable primes

module Main where

import Data.Char
import Data.List
import qualified Data.Map as M

main :: IO ()
main = print (sum (take 11 ns))
  where
    ns = filter truncatable primes

truncatable :: Integral a => a -> Bool
truncatable n =
    let ds = toDigits n
        forwards = tail (inits ds)
        backwards = tail (init (tails ds))
    in length ds > 1 &&
       all (prime . fromDigits) (forwards ++ backwards)

prime :: Integral a => a -> Bool
prime = (== 1) . length . factors

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

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral

fromDigits :: Integral a => [Int] -> a
fromDigits = fromIntegral . read . map intToDigit