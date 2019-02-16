module Test where

import Main hiding (main)

main :: IO ()
main = do
    testWordScore
    testLetterScore
    testTriangulars
    testCounts

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

testTriangulars :: IO ()
testTriangulars = test "triangulars"
    [ take 10 triangulars == [1,3,6,10,15,21,28,36,45,55]
    , triangulars !! 99 == sum [1..100]
    ]

testCounts :: IO ()
testCounts = test "counts"
    [ counts [1,3,3,7] == [(1,1),(3,2),(7,1)]
    , counts "root" == [('r',1),('o',2),('t',1)]
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
