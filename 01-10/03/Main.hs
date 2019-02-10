-- 3. Largest prime factor

module Main where

main :: IO ()
main = print (last (factors 600851475143))

factors :: Integral a => a -> [a]
factors n = loop n primes
  where
    loop n ps@(p:ps')
        | n < 2         = []
        | n < p^2       = [n]
        | divisible n p = p : loop (quot n p) ps
        | otherwise     = loop n ps'

primes :: Integral a => [a]
primes = 2 : filter prime [3,5..]

prime :: Integral a => a -> Bool
prime n = length (factors n) == 1

divisible :: Integral a => a -> a -> Bool
divisible n d = rem n d == 0
