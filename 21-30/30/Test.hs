module Test where

import Main hiding (main)

main :: IO ()
main = do
    testSumOfPowers
    testToDigits

testSumOfPowers :: IO ()
testSumOfPowers = test "sumOfPowers"
    [ take 3 (filter (sumOfPowers 4) [2..]) == [1634,8208,9474]
    ]

testToDigits :: IO ()
testToDigits = test "toDigits"
    [ toDigits 12345 == [1..5]
    , toDigits 987654321 == [9,8..1]
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
