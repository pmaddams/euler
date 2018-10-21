module Test where

import Main hiding (main)

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name

testRems :: IO ()
testRems = test "rems"
    [ take 3 (rems 6) == [1,4,4]
    , take 7 (rems 7) == [1,3,2,6,4,5,1]
    ]

testCycleLength :: IO ()
testCycleLength = test "cycleLength"
    [ cycleLength 6 == 1
    , cycleLength 7 == 6
    ]

main :: IO ()
main = do
    testRems
    testCycleLength
