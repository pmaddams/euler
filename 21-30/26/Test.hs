module Test where

import Main hiding (main)

main :: IO ()
main = testCycleLength

testCycleLength :: IO ()
testCycleLength = test "cycleLength"
    [ cycleLength 1 == 0
    , cycleLength 6 == 1
    , cycleLength 7 == 6
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
