-- 43. Sub-string divisibility

module Main where

import Control.Monad
import Data.Char
import Data.List

main :: IO ()
main = print (sum ns)
  where
    dss = map divisibleDigits [1,2,3,5,7,11,13,17]
    ns = map fromDigits (filter distinct (foldr1 mergeDigits dss))

divisibleDigits :: Integral a => a -> [[Int]]
divisibleDigits n =
    let ms = takeWhile (< 1000) (map (n*) [1..])
    in map (f . toDigits) ms
  where
    f ds = case (length ds) of
        1 -> 0:0:ds
        2 -> 0:ds
        3 -> ds
        _ -> error "divisibleDigits: too many digits"

mergeDigits :: [[Int]] -> [[Int]] -> [[Int]]
mergeDigits begins ends = concat (loop <$> begins <*> pure ends)
  where
    loop _ []                = []
    loop bs@(b:bs') (es:ess) =
        if bs' == take 2 es
        then (b:es) : loop bs ess
        else loop bs ess

distinct :: Eq a => [a] -> Bool
distinct xs = nub xs == xs

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral

fromDigits :: Integral a => [Int] -> a
fromDigits = fromIntegral . read . map intToDigit
