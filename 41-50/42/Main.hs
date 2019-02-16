-- 42. Coded triangle numbers

module Main where

import Data.Char
import Data.List

main :: IO ()
main = do
    words <- readCSV "42.csv"
    let cs = counts (map wordScore words)
    print (loop (sort cs) triangulars)
  where
    loop [] _    = 0
    loop cs@((w,n):cs') ts@(t:ts')
        | w < t  = loop cs' ts
        | w > t  = loop cs ts'
        | w == t = n + loop cs' ts'

wordScore :: String -> Int
wordScore = sum . map letterScore

letterScore :: Char -> Int
letterScore c
    | isUpper c = ord c - ord 'A' + 1
    | isLower c = ord c - ord 'a' + 1
    | otherwise = error "letterScore: invalid character"

counts :: Eq a => [a] -> [(a, Int)]
counts []       = []
counts xs@(y:_) = (y, length ys) : counts zs
  where
    (ys, zs) = partition (== y) xs

triangulars :: Integral a => [a]
triangulars = scanl1 (+) [1..]

readCSV :: Read a => FilePath -> IO [a]
readCSV name = do
    s <- readFile name
    return (read ("[" ++ s ++ "]"))
