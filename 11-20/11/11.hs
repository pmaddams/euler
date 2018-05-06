module Main where

import Control.Exception
import Data.Char

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

horProds :: Int -> [[Int]] -> [Int]
horProds _ []         = []
horProds len (ns:nss) = h' ns ++ horProds len nss
  where
    h' ns =
        if length ns < len
        then []
        else product (take len ns) : h' (tail ns)

checkHorProds :: IO ()
checkHorProds = check "horProds"
    [ horProds 2 [[1,2,3],[4,5,6],[7,8,9]] == [2,6,20,30,56,72]
    , horProds 3 [[1,2,3],[4,5,6],[7,8,9]] == [6,120,504]
    ]

vertProds :: Int -> [[Int]] -> [Int]
vertProds len nss =
    if length nss < len
    then []
    else v' (take len nss) ++ vertProds len (tail nss)
  where
    v' ([]:_) = []
    v' nss    = product (map head nss) : v' (map tail nss)

checkVertProds :: IO ()
checkVertProds = check "vertProds"
    [ vertProds 2 [[1,2,3],[4,5,6],[7,8,9]] == [4,10,18,28,40,54]
    , vertProds 3 [[1,2,3],[4,5,6],[7,8,9]] == [28,80,162]
    ]

diagProds :: [Int] -> Int -> [[Int]] -> [Int]
diagProds ixs len nss =
    if length nss < len
    then []
    else d' (take len nss) ++ diagProds ixs len (tail nss)
  where
    d' nss =
        if length (head nss) < len
        then []
        else product (zipWith ($) (map (flip (!!)) ixs) nss) : d' (map tail nss)

diagDownProds :: Int -> [[Int]] -> [Int]
diagDownProds len nss = diagProds [0..len-1] len nss

checkDiagDownProds :: IO ()
checkDiagDownProds = check "diagDownProds"
    [ diagDownProds 2 [[1,2,3],[4,5,6],[7,8,9]] == [5,12,32,45]
    , diagDownProds 3 [[1,2,3],[4,5,6],[7,8,9]] == [45]
    ]

diagUpProds :: Int -> [[Int]] -> [Int]
diagUpProds len nss = diagProds [len-1,len-2..] len nss

checkDiagUpProds :: IO ()
checkDiagUpProds = check "diagUpProds"
    [ diagUpProds 2 [[1,2,3],[4,5,6],[7,8,9]] == [8,15,35,48]
    , diagUpProds 3 [[1,2,3],[4,5,6],[7,8,9]] == [105]
    ]

linearProds :: Int -> [[Int]] -> [Int]
linearProds len nss = concat
    [ horProds len nss
    , vertProds len nss
    , diagDownProds len nss
    , diagUpProds len nss
    ]

test :: IO ()
test = do
    checkHorProds
    checkVertProds
    checkDiagDownProds
    checkDiagUpProds

main :: IO ()
main = do
    s <- readFile "11.txt"
    ss <- return (lines s)
    sss <- return (map words ss)
    nss <- return (map (map read) sss :: [[Int]])
    print (maximum (linearProds 4 nss))
