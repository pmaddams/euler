module Test where

import Main hiding (main)

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name

testRing :: IO ()
testRing = test "ring"
    [ ring 3 == [9,8..2]
    , ring 5 == [25,24..10]
    ]

testDiagonals :: IO ()
testDiagonals = test "diagonals"
    [ diagonals 3 == [9,7,5,3]
    , diagonals 5 == [25,21,17,13]
    ]

main :: IO ()
main = do
    testRing
    testDiagonals
