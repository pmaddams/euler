module Test where

import Main hiding (main)

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name

testChoose :: IO ()
testChoose = test "choose"
    [ 4 `choose` 2 == 6
    , 5 `choose` 3 == 10
    ]

testPaths :: IO ()
testPaths = test "paths"
    [ paths 1 == 2
    , paths 2 == 6
    ]

main :: IO ()
main = do
    testChoose
    testPaths
