module Test where

import Main hiding (main)

main :: IO ()
main = do
    testFraction
    testSimplify
    testDivisible
    testToDigits
    testFromDigits

testFraction :: IO ()
testFraction = test "Fraction"
    [ (F (-1) (-1)) == fromInteger 1
    , (F 1 2) + (F 3 4) == (F 5 4)
    , (F 2 3) * (F 3 4) == (F 1 2)
    ]

testSimplify :: IO ()
testSimplify = test "simplify"
    [ simplify (F 1 (-2)) == (F (-1) 2)
    , simplify (F 2 4) == (F 1 2)
    , simplify (F 4 2) == (F 2 1)
    ]

testDivisible :: IO ()
testDivisible = test "divisible"
    [ 4 `divisible` 2
    , not (7 `divisible` 3)
    , 10 `divisible` 5
    , not (13 `divisible` 7)
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
