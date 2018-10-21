module Test where

import Main hiding (main)

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name

testMergeRows :: IO ()
testMergeRows = test "mergeRows"
    [ mergeRows [1,2] [3,4,5] == [5,7]
    , mergeRows [6,7] [10,9,8] == [16,16]
    ]

testMaxTrianglePath :: IO ()
testMaxTrianglePath = test "maxTrianglePath"
    [ maxTrianglePath [[1],[2,3],[4,5,6]] == 10
    , maxTrianglePath [[1],[2,3],[6,5,4]] == 9
    ]

main :: IO ()
main = do
    testMergeRows
    testMaxTrianglePath
