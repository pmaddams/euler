module Test where

import Main hiding (main)

main :: IO ()
main = testAdjacent

testAdjacent :: IO ()
testAdjacent = test "adjacent"
    [ adjacent 2 [1..5] == [[1,2],[2,3],[3,4],[4,5]]
    , adjacent 3 [1..5] == [[1,2,3],[2,3,4],[3,4,5]]
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
