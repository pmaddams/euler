-- 22. Names scores

module Main where

import Data.Char
import Data.List

main :: IO ()
main = do
    names <- readCSV "22.csv"
    let ws = map wordScore (sort names)
        ns = zipWith (*) ws [1..]
    print (sum ns)

readCSV :: Read a => FilePath -> IO [a]
readCSV name = do
    s <- readFile name
    return (read ("[" ++ s ++ "]"))

wordScore :: String -> Int
wordScore = sum . map letterScore

letterScore :: Char -> Int
letterScore c
    | isUpper c = ord c - ord 'A' + 1
    | isLower c = ord c - ord 'a' + 1
    | otherwise = error "letterScore: invalid character"
