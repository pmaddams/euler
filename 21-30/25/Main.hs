-- 25. 1000-digit Fibonacci number

module Main where

import Data.List
import Data.Maybe

main :: IO ()
main = print (fromJust (findIndex p fibonacciNumbers))
  where
    p = (== 1000) . length . show

fibonacciNumbers :: Num a => [a]
fibonacciNumbers = ns
  where
    ns@(_:ns') = 0 : 1 : zipWith (+) ns ns'
