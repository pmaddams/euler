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
concatProduct n = loop [] [1..]
  where
    loop acc ~(m:ms)
        | not (distinct acc) = Nothing
        | sort acc == [1..9] = Just (fromDigits acc)
        | otherwise          = loop (acc ++ toDigits (n*m)) ms

distinct :: Eq a => [a] -> Bool
distinct xs = nub xs == xs

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral

fromDigits :: Integral a => [Int] -> a
fromDigits = fromIntegral . read . map intToDigit
