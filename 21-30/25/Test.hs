module Test where

import Main hiding (main)

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name

testFibonacci :: IO ()
testFibonacci = test "fibonacci"
    [ take 10 fibonacci == [1,1,2,3,5,8,13,21,34,55]
    , last (takeWhile (< 4000000) fibonacci) == 3524578
    ]

testToDigits :: IO ()
testToDigits = test "toDigits"
    [ toDigits 12345 == [1..5]
    , toDigits 987654321 == [9,8..1]
    ]

main :: IO ()
main = do
    testFibonacci
    testToDigits
