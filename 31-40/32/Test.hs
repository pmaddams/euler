module Test where

import Main hiding (main)

main :: IO ()
main = do
    testToDigits
    testUnique

testToDigits :: IO ()
testToDigits = test "toDigits"
    [ toDigits 12345 == [1..5]
    , toDigits 987654321 == [9,8..1]
    ]

testUnique :: IO ()
testUnique = test "unique"
    [ unique [1..5]
    , not (unique [1,2,3,2,5])
    , unique "cat"
    , not (unique "doog")
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
