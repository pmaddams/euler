module Main where

import Data.Char
import Data.List

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral

palindrome :: Eq a => [a] -> Bool
palindrome xs = xs == reverse xs

multiplyAll :: Num a => [a] -> [a]
multiplyAll []     = []
multiplyAll (n:ns) = (map (*n) (n:ns)) ++ multiplyAll ns

largestP :: Integral a => [a] -> Maybe a
largestP ns = find (palindrome . toDigits) $
    sortBy (flip compare) (multiplyAll ns)

main :: IO ()
main = case (largestP [100..999]) of
    Just n  -> print n
    Nothing -> putStrLn "none found"
