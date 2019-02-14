module Test where

import Main hiding (main)

main :: IO ()
main = testChange

testChange :: IO ()
testChange = test "change"
    [ change 1 [1,2,5] == 1
    , change 2 [1,2,5] == 2
    , change 5 [1,2,5] == 4
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
