module Test where

import Main hiding (main)

main :: IO ()
main = do
    testSumPrimeTwiceSquare
    testWithout
    testPrime
    testFactors
    testPrimes
    testDivisible

testSumPrimeTwiceSquare :: IO ()
testSumPrimeTwiceSquare = test "sumPrimeTwiceSquare"
    [ all sumPrimeTwiceSquare [9,15,21,25,27,33]
    ]

testWithout :: IO ()
testWithout = test "without"
    [ [1..10] `without` [2,4..10] == [1,3..9]
    , [1..10] `without` [1,3..9] == [2,4..10]
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

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
