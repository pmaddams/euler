-- 38. Pandigital multiples

module Main where

import Data.Char
import Data.List
import Data.Maybe

main :: IO ()
main = print (maximum ns)
  where
    ns = mapMaybe concatProduct [1..9876]

concatProduct :: Integral a => a -> Maybe a
concatProduct n = loop [1..] []
  where
    loop ~(m:ms) acc
        | not (unique acc)   = Nothing
        | sort acc == [1..9] = Just (fromDigits acc)
        | otherwise          = loop ms (acc ++ toDigits (n*m))

unique :: Eq a => [a] -> Bool
unique xs = nub xs == xs

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral

fromDigits :: Integral a => [Int] -> a
fromDigits = fromIntegral . read . map intToDigit
