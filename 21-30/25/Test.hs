module Test where

import Main hiding (main)

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name

testFibs :: IO ()
testFibs = test "fibs"
    [ take 10 fibs == [1,1,2,3,5,8,13,21,34,55]
    , last (takeWhile (< 4000000) fibs) == 3524578
    ]

testToDigits :: IO ()
testToDigits = test "toDigits"
    [ toDigits 12345 == [1..5]
    , toDigits 987654321 == [9,8..1]
    ]

main :: IO ()
main = do
    testFibs
    testToDigits
