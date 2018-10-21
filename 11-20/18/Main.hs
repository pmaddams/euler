module Main where

mergeRows :: [Int] -> [Int] -> [Int]
mergeRows upper lower = zipWith3 best upper lower (tail lower)
  where
    best u l l' = u + (max l l')

maxTrianglePath :: [[Int]] -> Int
maxTrianglePath nss = maximum (foldr1 mergeRows nss)

main :: IO ()
main = do
    s <- readFile "18.txt"
    ss <- return (lines s)
    sss <- return (map words ss)
    nss <- return (map (map read) sss :: [[Int]])
    print (maxTrianglePath nss)
