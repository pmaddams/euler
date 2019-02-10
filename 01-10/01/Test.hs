module Test where

import Main hiding (main)

main :: IO ()
main = do
    testAnyDivisible
    testDivisible

testAnyDivisible :: IO ()
testAnyDivisible = test "anyDivisible"
    [ anyDivisible 4 [2]
    , not (anyDivisible 7 [2, 3])
    , anyDivisible 10 [2, 3, 5]
    , not (anyDivisible 13 [2, 3, 5, 7])
    ]

testDivisible :: IO ()
testDivisible = test "divisible"
    [ divisible 4 2
    , not (divisible 7 3)
    , divisible 10 5
    , not (divisible 13 7)
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
