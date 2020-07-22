module Test where

import Main hiding (main)

main :: IO ()
main = do
    testDivisibleDigits
    testMergeDigits
    testDistinct
    testToDigits
    testFromDigits

testDivisibleDigits :: IO ()
testDivisibleDigits = test "divisibleDigits"
    [ divisibleDigits 30 ==
          [0,3,0] : [0,6,0] : [0,9,0] : map toDigits [120,150..990]
    , divisibleDigits 300 == map toDigits [300,600,900]
    ]

testMergeDigits :: IO ()
testMergeDigits = test "mergeDigits"
    [ mergeDigits [[1,2,3],[4,5,6]] [[2,3,5,7],[5,6,8,10]] ==
          [[1,2,3,5,7],[4,5,6,8,10]]
    , mergeDigits [[1,2,3],[4,5,6]] [[2,3,5,7],[2,3,4,5,6,7]] ==
          [[1,2,3,5,7],[1..7]]
    ]

testDistinct :: IO ()
testDistinct = test "distinct"
    [ distinct [1..5]
    , not (distinct [1,2,3,2,5])
    , distinct "cat"
    , not (distinct "doog")
    ]

testToDigits :: IO ()
testToDigits = test "toDigits"
    [ toDigits 12345 == [1..5]
    , toDigits 987654321 == [9,8..1]
    ]

testFromDigits :: IO ()
testFromDigits = test "fromDigits"
    [ fromDigits [1..5] == 12345
    , fromDigits [9,8..1] == 987654321
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
