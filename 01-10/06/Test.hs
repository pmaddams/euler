module Test where

import Main hiding (main)

main :: IO ()
main = do
    testSquareOfSum
    testSumOfSquares

testSquareOfSum :: IO ()
testSquareOfSum = test "squareOfSum"
    [ squareOfSum [1,2,3] == 36
    , squareOfSum [4,5,6] == 225
    ]

testSumOfSquares :: IO ()
testSumOfSquares = test "sumOfSquares"
    [ sumOfSquares [1,2,3] == 14
    , sumOfSquares [4,5,6] == 77
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
