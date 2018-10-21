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

testPalindrome :: IO ()
testPalindrome = test "palindrome"
    [ palindrome [1,2,3,2,1]
    , not (palindrome [1..5])
    ]

testMultiplyAll :: IO ()
testMultiplyAll = test "multiplyAll"
    [ multiplyAll [1..3] == [1,2,3,4,6,9]
    , multiplyAll [4..6] == [16,20,24,25,30,36]
    ]

testLargestP :: IO ()
testLargestP = test "largestP"
    [ largestP [10..99] == Just 9009
    , largestP [] == Nothing
    ]

main :: IO ()
main = do
    testToDigits
    testPalindrome
    testMultiplyAll
    testLargestP
