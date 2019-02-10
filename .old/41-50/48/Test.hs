module Test where

import Main hiding (main)

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name

testModExp :: IO ()
testModExp = test "modExp"
    [ modExp 5 3 3 == 2
    , modExp 2 3 5 == 3
    ]

main :: IO ()
main = testModExp
