module Test where

import Main hiding (main)

main :: IO ()
main = do
    testConcatProduct
    testUnique
    testToDigits
    testFromDigits

testConcatProduct :: IO ()
testConcatProduct = test "concatProduct"
    [ concatProduct 192 == Just 192384576
    , concatProduct 9 == Just 918273645
    ]

testUnique :: IO ()
testUnique = test "unique"
    [ unique [1..5]
    , not (unique [1,2,3,2,5])
    , unique "cat"
    , not (unique "doog")
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
