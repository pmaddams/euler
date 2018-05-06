module Main where

check :: String -> [Bool] -> IO ()
check name tests =
    if not (and tests)
    then error name
    else return ()

pentagons :: [Int]
pentagons = [n*(3*n-1) `div` 2 | n <- [1..]]

checkPentagons :: IO ()
checkPentagons = check "pentagons"
    [ take 5 pentagons == [1,5,12,22,35]
    , 1001 `elem` pentagons
    ]

pentagonal :: Int -> Bool
pentagonal n =
    let r = sqrt (fromIntegral (24*n + 1))
        (ri, rf) = properFraction r
    in rf == 0 && ri `mod` 6 == 5

checkPentagonal :: IO ()
checkPentagonal = check "pentagonal"
    [ all pentagonal [1,5,12,22,35]
    , not (pentagonal 48)
    ]

test :: IO ()
test = do
    checkPentagons
    checkPentagonal

main :: IO ()
main = print $ head $
    [ (k-j)
    | k <- pentagons
    , j <- takeWhile (< k) pentagons
    , pentagonal (k-j)
    , pentagonal (j+k)
    ]
