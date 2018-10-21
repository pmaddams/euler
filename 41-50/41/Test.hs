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

testFromDigits :: IO ()
testFromDigits = test "fromDigits"
    [ fromDigits [1..5] == 12345
    , fromDigits [9,8..1] == 987654321
    ]

testPandigitals :: IO ()
testPandigitals = test "pandigitals"
    [ head pandigitals == 987654321
    , last pandigitals == 1
    ]

main :: IO ()
main = do
    testModExp
    testPrime
    testFromDigits
    testPandigitals
