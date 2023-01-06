module Test where

import Main hiding (main)

main :: IO ()
main = testWaysToMakeChange

testWaysToMakeChange :: IO ()
testWaysToMakeChange = test "waysToMakeChange"
    [ waysToMakeChange 1 [1,2,5] == 1
    , waysToMakeChange 2 [1,2,5] == 2
    , waysToMakeChange 5 [1,2,5] == 4
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
