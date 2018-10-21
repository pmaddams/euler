module Test where

import Main hiding (main)

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name

testToDigits :: IO ()
testToDigits = test "toDigits"
    [ toDigits 12345 == [1..5]
    , toDigits 987654321 == [9,8..1]
    ]

testFactorial :: IO ()
testFactorial = test "factorial"
    [ and (map ((==1) . factorial) [-1..1])
    , factorial 10 == 3628800
    ]

testSumDigitFactorials :: IO ()
testSumDigitFactorials = test "sumFactorialDigits"
    [ sumDigitFactorials 123 == 9
    , sumDigitFactorials 145 == 145
    ]

main :: IO ()
main = do
    testToDigits
    testSumDigitFactorials
