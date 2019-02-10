module Test where

import Main hiding (main)

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name

testCountChange :: IO ()
testCountChange = test "countChange"
    [ countChange 5 [1,5,10] == 2
    , countChange 10 [1,5,10] == 4
    ]

main :: IO ()
main = testCountChange
