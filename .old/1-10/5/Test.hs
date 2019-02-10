module Test where

import Main hiding (main)

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name

testLeastMultiple :: IO ()
testLeastMultiple = test "leastMultiple"
    [ leastMultiple [1..10] == 2520
    , leastMultiple [2,3,4,6] == 12
    ]

main :: IO ()
main = testLeastMultiple
