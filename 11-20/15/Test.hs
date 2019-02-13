module Test where

import Main hiding (main)

main :: IO ()
main = do
    testPaths
    testChoose

testPaths :: IO ()
testPaths = test "paths"
    [ paths 1 == 2
    , paths 2 == 6
    , paths 3 == 20
    ]

testChoose :: IO ()
testChoose = test "choose"
    [ 2 `choose` 1 == 2
    , 4 `choose` 2 == 6
    , 6 `choose` 3 == 20
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
