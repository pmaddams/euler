module Main where

import Control.Exception
import Data.Char
import Data.List

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

letterScore :: Char -> Int
letterScore c
    | not (isAlpha c) = 0
    | isUpper c       = ord c - ord 'A' + 1
    | isLower c       = ord c - ord 'a' + 1

checkLetterScore :: IO ()
checkLetterScore = check "letterScore"
    [ letterScore 'a' == 1
    , letterScore 'Z' == 26
    ]

wordScore :: String -> Int
wordScore = sum . (map letterScore)

checkWordScore :: IO ()
checkWordScore = check "wordScore"
    [ wordScore "COLIN" == 53
    , wordScore "Pavan" == 54
    ]

triangles :: [Int]
triangles = scanl1 (+) [1..]

checkTriangles :: IO ()
checkTriangles = check "triangles"
    [ take 10 triangles == [1,3,6,10,15,21,28,36,45,55]
    , triangles !! 99 == sum [1..100]
    ]

runs :: Eq a => [a] -> [(a, Int)]
runs = map (\xs -> (head xs, length xs)) . group

checkRuns :: IO ()
checkRuns = check "runs"
    [ runs [1,2,2,3,3,3] == [(1,1),(2,2),(3,3)]
    , runs [1,1,2,2,2,3] == [(1,2),(2,3),(3,1)]
    ]

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

checkNumTriangleWords :: IO ()
checkNumTriangleWords = check "numTriangleWords"
    [ numTriangleWords ["sky", "abc", "def"] == 3
    , numTriangleWords ["ghi", "jkl", "mno"] == 0
    ]

extractWords :: String -> [String]
extractWords cs = e' cs []
  where
    e' "" acc       = [acc]
    e' (c:cs) acc
        | c == ','  = acc : e' cs []
        | isAlpha c = e' cs (acc ++ [c])
        | otherwise = e' cs acc

checkExtractWords :: IO ()
checkExtractWords = check "extractWords"
    [ extractWords "hello, world" == ["hello","world"]
    , extractWords "hello,,w,orld" == ["hello","","w","orld"]
    ]

test :: IO ()
test = do
    checkLetterScore
    checkWordScore
    checkTriangles
    checkRuns
    checkNumTriangleWords
    checkExtractWords

main :: IO ()
main = do
    s <- readFile "42.txt"
    ss <- return (extractWords s)
    print (numTriangleWords ss)
