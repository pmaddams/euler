-- 4. Largest palindrome product

module Main where

import Data.List
import Data.Maybe

main :: IO ()
main = print (fromJust (find palindrome ns))
  where
    ns = sortReverse (products [100..999])

palindrome :: Show a => a -> Bool
palindrome x =
    let s = show x
    in s == reverse s

sortReverse :: Ord a => [a] -> [a]
sortReverse = sortBy (flip compare)

products :: Num a => [a] -> [a]
products []         = []
products ns@(n:ns') = map (*n) ns ++ products ns'
