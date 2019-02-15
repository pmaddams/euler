module Test where

import Main hiding (main)

main :: IO ()
main = do
    testPerimeters
    testCounts
    testBest

testPerimeters :: IO ()
testPerimeters = test "perimeters"
    [ take 2 perimeters == [3+4+5, 5+12+13]
    ]

testCounts :: IO ()
testCounts = test "counts"
    [ counts [1,3,3,7] == [(1,1),(3,2),(7,1)]
    , counts "root" == [('r',1),('o',2),('t',1)]
    ]

testBest :: IO ()
testBest = test "best"
    [ best [("a", 2), ("b", 3), ("c", 1)] == "b"
    , best [(2, "a"), (3, "b"), (1, "c")] == 1
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
