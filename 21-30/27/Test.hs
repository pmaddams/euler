module Test where

import Main hiding (main)

main :: IO ()
main = do
    testQuadraticPrimes
    testPrime
    testFactors
    testPrimes
    testAnyDivisible
    testDivisible
    testBest

testQuadraticPrimes :: IO ()
testQuadraticPrimes = test "quadraticPrimes"
    [ length (quadraticPrimes 1 41) == 40
    , length (quadraticPrimes (-79) 1601) == 80
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

testAnyDivisible :: IO ()
testAnyDivisible = test "anyDivisible"
    [ anyDivisible 4 [2]
    , not (anyDivisible 7 [2, 3])
    , anyDivisible 10 [2, 3, 5]
    , not (anyDivisible 13 [2, 3, 5, 7])
    ]

testDivisible :: IO ()
testDivisible = test "divisible"
    [ divisible 4 2
    , not (divisible 7 3)
    , divisible 10 5
    , not (divisible 13 7)
    ]

testBest :: IO ()
testBest = test "best"
    [ best [("a", 2), ("b", 3), ("c", 1)] == "b"
    , best [(2, "a"), (3, "b"), (1, "c")] == 1
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
