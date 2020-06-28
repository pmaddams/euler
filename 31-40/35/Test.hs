module Test where

import Main hiding (main)

main :: IO ()
main = do
    testCircular
    testRotations
    testPrime
    testFactors
    testPrimes
    testDivisible
    testToDigits
    testFromDigits

testCircular :: IO ()
testCircular = test "circular"
    [ filter circular (takeWhile (< 100) primes) ==
          [2,3,5,7,11,13,17,31,37,71,73,79,97]
    ]

testRotations :: IO ()
testRotations = test "rotations"
    [ rotations [1,2,3] == [[1,2,3],[2,3,1],[3,1,2]]
    , rotations "abc" == ["abc","bca","cab"]
    ]

testPrime :: IO ()
testPrime = test "prime"
    [ all prime [2,3,5,7,11]
    , not (any prime [0,1,4,9,15])
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
