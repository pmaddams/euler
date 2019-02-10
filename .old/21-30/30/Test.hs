module Test where

import Main hiding (main)

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name

testIsSumOfPowers :: IO ()
testIsSumOfPowers = test "isSumOfPowers"
    [ not (isSumOfPowers 4 1234)
    , and (map (isSumOfPowers 4) [1634,8208,9474])
    ]

main :: IO ()
main = testIsSumOfPowers
