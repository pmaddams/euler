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

testChampernowneDigits :: IO ()
testChampernowneDigits = test "champernowneDigits"
    [ take 9 champernowneDigits == [1..9]
    , champernowneDigits !! 11 == 1
    ]

main :: IO ()
main = do
    testToDigits
    testChampernowneDigits
