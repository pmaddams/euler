module Test where

import Main hiding (main)

main :: IO ()
main = do
    testTruncatable
    testPrime
    testFactors
    testPrimes
    testAnyDivisible
    testDivisible
    testToDigits
    testFromDigits

testTruncatable :: IO ()
testTruncatable = test "truncatable"
    [ truncatable 3797
    , not (any truncatable [2,3,5,7])
    ]

testPrime :: IO ()
testPrime = test "prime"
    [ all prime [2,3,5,7,11]
    , not (any prime [1,4,6,8,9])
    ]

testFactors :: IO ()
testFactors = test "factors"
    [ factors 4 == [2,2]
    , factors 5 == [5]
    , factors 6 == [2,3]
    , factors 7 == [7]
    ]

testPrimes :: IO ()
testPrimes = test "primes"
    [ take 5 primes == [2,3,5,7,11]
    , primes !! 99 == 541
    ]

testDivisible :: IO ()
testDivisible = test "divisible"
    [ divisible 4 2
    , not (divisible 7 3)
    , divisible 10 5
    , not (divisible 13 7)
    ]

testAnyDivisible :: IO ()
testAnyDivisible = test "anyDivisible"
    [ anyDivisible 4 [2]
    , not (anyDivisible 7 [2, 3])
    , anyDivisible 10 [2, 3, 5]
    , not (anyDivisible 13 [2, 3, 5, 7])
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

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
