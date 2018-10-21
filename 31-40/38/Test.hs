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

testMaybePandigitalMultiple :: IO ()
testMaybePandigitalMultiple = test "maybePandigitalMultiple"
    [ maybePandigitalMultiple 192 == Just 192384576
    , maybePandigitalMultiple 9 == Just 918273645
    ]

main :: IO ()
main = do
    testToDigits
    testFromDigits
    testMaybePandigitalMultiple
