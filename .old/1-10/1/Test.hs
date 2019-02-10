module Test where

import Main hiding (main)

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name

testAnyOf :: IO ()
testAnyOf =
    let fs = [(== 3), (== 5)]
    in test "anyOf"
        [ anyOf fs 3
        , anyOf fs 5
        , not (anyOf fs 6)
        ]

testDivis :: IO ()
testDivis = test "divis"
    [ 6 `divis` 3
    , 10 `divis` 5
    , not (10 `divis` 3)
    ]

main :: IO ()
main = do
    testAnyOf
    testDivis
