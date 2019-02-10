-- 2. Even Fibonacci numbers

module Main where

main :: IO ()
main = print (sum (takeWhile (<= 4000000) ns))
  where
    ns = filter even fibonacci

fibonacci :: Num a => [a]
fibonacci = ns
  where
    ns@(_:ns') = 0 : 1 : zipWith (+) ns ns'
