-- 4. Largest palindrome product

module Main where

import Data.Char
import Data.List
import Data.Maybe

main :: IO ()
main = print (fromJust (find (palindrome . toDigits) ns))
  where
    ns = sortReverse (products [100..999])

palindrome :: Eq a => [a] -> Bool
palindrome xs = xs == reverse xs

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral

sortReverse :: Ord a => [a] -> [a]
sortReverse = sortBy (flip compare)

products :: Num a => [a] -> [a]
products []         = []
products ns@(n:ns') = map (n*) ns ++ products ns'
