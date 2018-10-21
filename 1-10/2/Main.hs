module Main where

fibonacci :: [Integer]
fibonacci = 1 : 1 : zipWith (+) fibonacci (tail fibonacci)
  
main :: IO ()
main = print (sum (filter even (takeWhile (< 4000000) fibonacci)))
