module Main where

import Data.Char
import Data.List

letterScore :: Char -> Int
letterScore c
    | not (isAlpha c) = 0
    | isUpper c       = ord c - ord 'A' + 1
    | isLower c       = ord c - ord 'a' + 1

wordScore :: String -> Int
wordScore = sum . (map letterScore)

triangles :: [Int]
triangles = scanl1 (+) [1..]

runs :: Eq a => [a] -> [(a, Int)]
runs = map (\xs -> (head xs, length xs)) . group

numTriangleWords :: [String] -> Int
numTriangleWords ws =
    let rs = runs (sort (map wordScore ws))
    in t' rs triangles 0
  where
    t' [] (n:ns) acc = acc
    t' (p@(v,c):ps) (n:ns) acc
        | v < n      = t' ps (n:ns) acc
        | v > n      = t' (p:ps) ns acc
        | v == n     = t' ps ns (acc+c)

extractWords :: String -> [String]
extractWords cs = e' cs []
  where
    e' "" acc       = [acc]
    e' (c:cs) acc
        | c == ','  = acc : e' cs []
        | isAlpha c = e' cs (acc ++ [c])
        | otherwise = e' cs acc

main :: IO ()
main = do
    s <- readFile "42.txt"
    ss <- return (extractWords s)
    print (numTriangleWords ss)
