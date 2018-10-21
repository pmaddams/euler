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

multiplyPosition :: [Int] -> [Int]
multiplyPosition = zipWith (*) [1..]

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
    s <- readFile "22.txt"
    ss <- return (sort (extractWords s))
    print (sum (multiplyPosition (map wordScore ss)))
