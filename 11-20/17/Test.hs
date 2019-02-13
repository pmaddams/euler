module Test where

import Main hiding (main)

main :: IO ()
main = testSay

testSay :: IO ()
testSay = test "say"
    [
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
