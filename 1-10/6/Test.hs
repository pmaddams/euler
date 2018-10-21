module Test where

import Main hiding (main)

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name

testDiffSumSquares :: IO ()
testDiffSumSquares = test "diffSumSquares"
    [ diffSumSquares [1..10] == 2640
    , diffSumSquares [20,30] == 1200
    ]

main :: IO ()
main = testDiffSumSquares
