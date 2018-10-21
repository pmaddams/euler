module Test where

import Main hiding (main)

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name

testPentagons :: IO ()
testPentagons = test "pentagons"
    [ take 5 pentagons == [1,5,12,22,35]
    , 1001 `elem` pentagons
    ]

testPentagonal :: IO ()
testPentagonal = test "pentagonal"
    [ all pentagonal [1,5,12,22,35]
    , not (pentagonal 48)
    ]

main :: IO ()
main = do
    testPentagons
    testPentagonal
