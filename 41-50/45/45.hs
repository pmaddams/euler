module Main where

import Data.List

check :: String -> [Bool] -> IO ()
check name tests =
    if and tests
    then return ()
    else error name

hexagons :: [Int]
hexagons = [n*(2*n-1) | n <- [1..]]

checkHexagons :: IO ()
checkHexagons = check "hexagons"
    [ take 5 hexagons == [1,6,15,28,45]
    , 3003 `elem` hexagons
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
    checkHexagons
    checkPentagonal

main :: IO ()
main =
    let hs = dropWhile (<= 40755) hexagons
    in case (find pentagonal hs) of
           Just n -> print n
