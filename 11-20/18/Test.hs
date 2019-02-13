module Test where

import Main hiding (main)

main :: IO ()
main = testMergeRows

testMergeRows :: IO ()
testMergeRows = test "mergeRows"
    [ mergeRows [1] [2,3] == [4]
    , mergeRows [1,2] [5,4,3] == [6,6]
    , mergeRows [1,2,3] [5,4,7,6] == [6,9,10]
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
