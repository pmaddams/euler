module Test where

import Main hiding (main)

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name

testHexagons :: IO ()
testHexagons = test "hexagons"
    [ take 5 hexagons == [1,6,15,28,45]
    , 3003 `elem` hexagons
    ]

testPentagonal :: IO ()
testPentagonal = test "pentagonal"
    [ all pentagonal [1,5,12,22,35]
    , not (pentagonal 48)
    ]

main :: IO ()
main = do
    testHexagons
    testPentagonal
