module Test where

import Main hiding (main)

main :: IO ()
main = do
    testFactorial
    testToDigits

testFactorial :: IO ()
testFactorial = test "factorial"
    [ all (== 1) (map factorial [-1..1])
    , factorial 10 == 3628800
    ]

testToDigits :: IO ()
testToDigits = test "toDigits"
    [ toDigits 12345 == [1..5]
    , toDigits 987654321 == [9,8..1]
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
