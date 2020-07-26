module Test where

import Main hiding (main)

main :: IO ()
main = do
    testCycleLength
    testBest

testCycleLength :: IO ()
testCycleLength = test "cycleLength"
    [ cycleLength 1 == 0
    , cycleLength 6 == 1
    , cycleLength 7 == 6
    ]

testBest :: IO ()
testBest = test "best"
    [ best [("a",2),("b",3),("c",1)] == "b"
    , best [(2,"a"),(3,"b"),(1,"c")] == 1
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
