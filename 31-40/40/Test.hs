module Test where

import Main hiding (main)

main :: IO ()
main = do
    testChampernowne
    testToDigits

testChampernowne :: IO ()
testChampernowne = test "champernowne"
    [ take 9 champernowne == [1..9]
    , champernowne !! 11 == 1
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
