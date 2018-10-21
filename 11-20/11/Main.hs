module Main where

import Data.Char

horProds :: Int -> [[Int]] -> [Int]
horProds _ []         = []
horProds len (ns:nss) = h' ns ++ horProds len nss
  where
    h' ns =
        if length ns < len
        then []
        else product (take len ns) : h' (tail ns)

vertProds :: Int -> [[Int]] -> [Int]
vertProds len nss =
    if length nss < len
    then []
    else v' (take len nss) ++ vertProds len (tail nss)
  where
    v' ([]:_) = []
    v' nss    = product (map head nss) : v' (map tail nss)

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

diagUpProds :: Int -> [[Int]] -> [Int]
diagUpProds len nss = diagProds [len-1,len-2..] len nss

linearProds :: Int -> [[Int]] -> [Int]
linearProds len nss = concat
    [ horProds len nss
    , vertProds len nss
    , diagDownProds len nss
    , diagUpProds len nss
    ]

main :: IO ()
main = do
    s <- readFile "11.txt"
    ss <- return (lines s)
    sss <- return (map words ss)
    nss <- return (map (map read) sss :: [[Int]])
    print (maximum (linearProds 4 nss))
