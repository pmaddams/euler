module Test where

import Main hiding (main)

main :: IO ()
main = do
    testAnyDivisible
    testDivisible

testAnyDivisible :: IO ()
testAnyDivisible = test "anyDivisible"
    [ 4 `anyDivisible` [2]
    , not (7 `anyDivisible` [2, 3])
    , 10 `anyDivisible` [2, 3, 5]
    , not (13 `anyDivisible` [2, 3, 5, 7])
    ]

testDivisible :: IO ()
testDivisible = test "divisible"
    [ 4 `divisible` 2
    , not (7 `divisible` 3)
    , 10 `divisible` 5
    , not (13 `divisible` 7)
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
