-- 6. Sum square difference

module Main where

import Control.Arrow

main :: IO ()
main = print (n - m)
  where
    (n, m) = (squareOfSum &&& sumOfSquares) [1..100]

squareOfSum :: Num a => [a] -> a
squareOfSum = (^2) . sum

sumOfSquares :: Num a => [a] -> a
sumOfSquares = sum . map (^2)
