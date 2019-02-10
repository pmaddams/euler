module Test where

import Main hiding (main)

main :: IO ()
main = testSmallestMultiple

testSmallestMultiple :: IO ()
testSmallestMultiple = test "smallestMultiple"
    [ smallestMultiple [1..5] == 60
    , smallestMultiple [1..10] == 2520
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
