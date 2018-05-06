module Main where

check :: String -> [Bool] -> IO ()
check name tests =
    if not (and tests)
    then error name
    else return ()

ring :: Int -> [Int]
ring 1 = [1]
ring n =
    if odd n
    then take (4*n - 4) [n^2, n^2-1..]
    else error "ring dimensions must be odd"

checkRing :: IO ()
checkRing = check "ring"
    [ ring 3 == [9,8..2]
    , ring 5 == [25,24..10]
    ]

diagonals :: Int -> [Int]
diagonals 1 = [1]
diagonals n =
    let ixs = zip [0..] (ring n)
    in [x | (i, x) <- ixs, i `mod` (n-1) == 0]

checkDiagonals :: IO ()
checkDiagonals = check "diagonals"
    [ diagonals 3 == [9,7,5,3]
    , diagonals 5 == [25,21,17,13]
    ]

test :: IO ()
test = do
    checkRing
    checkDiagonals

main :: IO ()
main = print (sum (concatMap diagonals [1,3..1001]))
