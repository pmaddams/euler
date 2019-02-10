module Test where

import Main hiding (main)

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name

testPrimes :: IO ()
testPrimes = test "primes"
    [ take 6 primes == [2,3,5,7,11,13]
    , primes !! 999 == 7919
    ]

testModExp :: IO ()
testModExp = test "modExp"
    [ modExp 5 3 3 == 2
    , modExp 2 3 5 == 3
    ]

testPrime :: IO ()
testPrime = test "prime"
    [ all prime [2,3,5,7,11,13]
    , not (any prime [-1,0,1,4,9])
    ]

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

testUniqueDigits :: IO ()
testUniqueDigits = test "uniqueDigits"
    [ uniqueDigits [123,132,213,231,312,321] == [123]
    , uniqueDigits [123,231,456,465,789,987] == [123,456,789]
    ]

testDigitPermutationOf :: IO ()
testDigitPermutationOf = test "digitPermutationOf"
    [ 123 `digitPermutationOf` 321
    , not (123 `digitPermutationOf` 311)
    ]

main :: IO ()
main = do
    testPrimes
    testModExp
    testPrime
    testToDigits
    testFromDigits
    testUniqueDigits
    testDigitPermutationOf
