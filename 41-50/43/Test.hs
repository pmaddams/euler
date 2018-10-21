module Test where

import Main hiding (main)

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name

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

testDistinct :: IO ()
testDistinct = test "distinct"
    [ distinct [1,2,3]
    , not (distinct [2,2,3])
    ]

testDivisibleGroups :: IO ()
testDivisibleGroups = test "divisibleGroups"
    [ divisibleGroups 30 ==
          [0,3,0] : [0,6,0] : [0,9,0] : map toDigits [120,150..990]
    , divisibleGroups 300 == map toDigits [300,600,900]
    ]

testMergeGroups :: IO ()
testMergeGroups = test "mergeGroups"
    [ mergeGroups [[1,2,3],[4,5,6]] [[2,3,5,7],[5,6,8,10]] ==
          [[1,2,3,5,7],[4,5,6,8,10]]
    , mergeGroups [[1,2,3],[4,5,6]] [[2,3,5,7],[2,3,4,5,6,7]] ==
          [[1,2,3,5,7],[1..7]]
    ]

main :: IO ()
main = do
    testToDigits
    testFromDigits
    testDistinct
    testDivisibleGroups
    testMergeGroups
