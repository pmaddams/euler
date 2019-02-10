module Main where

ring :: Int -> [Int]
ring 1 = [1]
ring n =
    if odd n
    then take (4*n - 4) [n^2, n^2-1..]
    else error "ring dimensions must be odd"

diagonals :: Int -> [Int]
diagonals 1 = [1]
diagonals n =
    let ixs = zip [0..] (ring n)
    in [x | (i, x) <- ixs, i `mod` (n-1) == 0]

main :: IO ()
main = print (sum (concatMap diagonals [1,3..1001]))
