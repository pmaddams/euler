module Test where

import Main hiding (main)

main :: IO ()
main = do
    testAmicable
    testDivisors
    testFactors
    testPrimes
    testDivisible

testAmicable :: IO ()
testAmicable = test "amicable"
    [ amicable 220
    , amicable 284
    , not (any amicable [1..10])
    ]

testDivisors :: IO ()
testDivisors = test "divisors"
    [ divisors 4 == [1,2,4]
    , divisors 5 == [1,5]
    , divisors 6 == [1,2,3,6]
    , divisors 7 == [1,7]
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
    [ 4 `divisible` 2
    , not (7 `divisible` 3)
    , 10 `divisible` 5
    , not (13 `divisible` 7)
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
