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

testToBits :: IO ()
testToBits = test "toBits"
    [ toBits 4 == [1,0,0]
    , toBits 5 == [1,0,1]
    ]

testPalindrome :: IO ()
testPalindrome = test "palindrome"
    [ palindrome [1,2,3,2,1]
    , not (palindrome [1..5])
    ]

testDoublePalindrome :: IO ()
testDoublePalindrome = test "doublePalindrome"
    [ not (doublePalindrome 4)
    , doublePalindrome 585
    ]

main :: IO ()
main = do
    testToDigits
    testToBits
    testPalindrome
    testDoublePalindrome
