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

testRotations :: IO ()
testRotations = test "rotations"
    [ rotations [1,2,3] ==
        [[2,3,1],[3,1,2],[1,2,3]]
    , rotations "hello" ==
        ["elloh","llohe","lohel","ohell","hello"]
    ]

testPrimeRotations :: IO ()
testPrimeRotations = test "primeRotations"
    [ primeRotations 197
    , not (primeRotations 101)
    ]

main :: IO ()
main = do
    testPrimes
    testModExp
    testPrime
    testToDigits
    testFromDigits
    testRotations
    testPrimeRotations