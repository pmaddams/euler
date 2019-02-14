module Test where

import Main hiding (main)

main :: IO ()
main = do
    testPalindrome
    testToDigits
    testSortReverse
    testProducts

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

testSortReverse :: IO ()
testSortReverse = test "sortReverse"
    [ sortReverse [1..10] == [10,9..1]
    , sortReverse "sortReverse" == "vtssrroeeeR"
    ]

testProducts :: IO ()
testProducts = test "products"
    [ products [1,2,3] == [1,2,3,4,6,9]
    , products [4,5,6] == [16,20,24,25,30,36]
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
