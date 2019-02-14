module Test where

import Main hiding (main)

main :: IO ()
main = testPowers

testPowers :: IO ()
testPowers = test "powers"
    [ powers [2] [1..4] == [2,4,8,16]
    , powers [1,2] [3,4] == [1,1,8,16]
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
