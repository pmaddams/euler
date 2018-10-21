module Test where

import Main hiding (main)

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name

testLexPerms :: IO ()
testLexPerms = test "lexPerms"
    [ "3124" `elem` (lexPerms [1..4])
    , lexPerms [0..2] == ["012","021","102","120","201","210"]
    ]

main :: IO ()
main = testLexPerms
