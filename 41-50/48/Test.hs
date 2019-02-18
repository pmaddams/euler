module Test where

import Main hiding (main)

main :: IO ()
main = testExpmod

testExpmod :: IO ()
testExpmod = test "expmod"
    [ expmod 5 3 3 == 2
    , expmod 2 3 5 == 3
    , expmod 1000 1000 9 == 1
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
