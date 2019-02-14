module Test where

import Main hiding (main)

main :: IO ()
main = testSpiral

testSpiral :: IO ()
testSpiral = test "spiral"
    [ take 9 spiral == [1,3,5,7,9,13,17,21,25]
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
