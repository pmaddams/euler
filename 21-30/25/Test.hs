module Test where

import Main hiding (main)

main :: IO ()
main = testFibonacci

testFibonacci :: IO ()
testFibonacci = test "fibonacci"
    [ take 5 fibonacci == [0,1,1,2,3]
    , fibonacci !! 20 == 6765
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
