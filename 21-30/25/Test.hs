module Test where

import Main hiding (main)

main :: IO ()
main = testFibonacciNumbers

testFibonacciNumbers :: IO ()
testFibonacciNumbers = test "fibonacciNumbers"
    [ take 5 fibonacciNumbers == [0,1,1,2,3]
    , fibonacciNumbers !! 20 == 6765
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
