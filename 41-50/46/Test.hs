module Test where

import Main hiding (main)

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name

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

testSumOfPrimeAndDoubleSquare :: IO ()
testSumOfPrimeAndDoubleSquare = test "sumOfPrimeAndDoubleSquare"
    [ all sumOfPrimeAndDoubleSquare [9,15,21,25,27,33]
    , not (any sumOfPrimeAndDoubleSquare [1,2,3,6,8,12])
    ]

main :: IO ()
main = do
    testModExp
    testPrime
    testSumOfPrimeAndDoubleSquare
