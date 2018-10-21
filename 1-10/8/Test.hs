module Test where

import Main hiding (main)

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name

testAdjacent :: IO ()
testAdjacent = test "adjacent"
    [ adjacent 2 [1..5] == [[1,2],[2,3],[3,4],[4,5]]
    , adjacent 3 [1..5] == [[1,2,3],[2,3,4],[3,4,5]]
    ]

testMaxAdjacent :: IO ()
testMaxAdjacent = test "maxAdjacent"
    [ maxAdjacent 4 [1,9,9,8,9,0] == Just 5832
    , maxAdjacent 7 [1,9,9,8,9,0] == Nothing
    ]

main :: IO ()
main = do
    testAdjacent
    testMaxAdjacent
