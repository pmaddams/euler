module Test where

import Main hiding (main)

main :: IO ()
main = do
    testIsDigitPowerSum
    testToDigits

testIsDigitPowerSum :: IO ()
testIsDigitPowerSum = test "isDigitPowerSum"
    [ take 3 (filter (isDigitPowerSum 4) [2..]) == [1634,8208,9474]
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
