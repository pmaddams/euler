module Test where

import Main hiding (main)

main :: IO ()
main = do
    testPalindrome
    testToDigits
    testToBits

testPalindrome :: IO ()
testPalindrome = test "palindrome"
    [ palindrome "abba"
    , not (palindrome "abab")
    , palindrome [1,2,3,2,1]
    , not (palindrome [1..5])
    ]

testToDigits :: IO ()
testToDigits = test "toDigits"
    [ toDigits 12345 == [1..5]
    , toDigits 987654321 == [9,8..1]
    ]

testToBits :: IO ()
testToBits = test "toBits"
    [ toBits 1 == [1]
    , toBits 2 == [0,1]
    , toBits 5 == [1,0,1]
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
