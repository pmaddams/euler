module Test where

import Main hiding (main)

main :: IO ()
main = testModPow

testModPow :: IO ()
testModPow = test "modPow"
    [ modPow 5 3 3 == 2
    , modPow 2 3 5 == 3
    , modPow 1000 1000 9 == 1
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
