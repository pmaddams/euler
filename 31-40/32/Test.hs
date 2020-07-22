module Test where

import Main hiding (main)

main :: IO ()
main = do
    testToDigits
    testDistinct

testToDigits :: IO ()
testToDigits = test "toDigits"
    [ toDigits 12345 == [1..5]
    , toDigits 987654321 == [9,8..1]
    ]

testDistinct :: IO ()
testDistinct = test "distinct"
    [ distinct [1..5]
    , not (distinct [1,2,3,2,5])
    , distinct "cat"
    , not (distinct "doog")
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
