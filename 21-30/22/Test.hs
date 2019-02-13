module Test where

import Main hiding (main)

main :: IO ()
main = do
    testWordScore
    testLetterScore

testLetterScore :: IO ()
testLetterScore = test "letterScore"
    [ letterScore 'a' == 1
    , letterScore 'Z' == 26
    ]

testWordScore :: IO ()
testWordScore = test "wordScore"
    [ wordScore "COLIN" == 53
    , wordScore "Pavan" == 54
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
