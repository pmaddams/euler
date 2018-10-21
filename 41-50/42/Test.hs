module Test where

import Main hiding (main)

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name

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

testTriangles :: IO ()
testTriangles = test "triangles"
    [ take 10 triangles == [1,3,6,10,15,21,28,36,45,55]
    , triangles !! 99 == sum [1..100]
    ]

testRuns :: IO ()
testRuns = test "runs"
    [ runs [1,2,2,3,3,3] == [(1,1),(2,2),(3,3)]
    , runs [1,1,2,2,2,3] == [(1,2),(2,3),(3,1)]
    ]

testNumTriangleWords :: IO ()
testNumTriangleWords = test "numTriangleWords"
    [ numTriangleWords ["sky", "abc", "def"] == 3
    , numTriangleWords ["ghi", "jkl", "mno"] == 0
    ]

testExtractWords :: IO ()
testExtractWords = test "extractWords"
    [ extractWords "hello, world" == ["hello","world"]
    , extractWords "hello,,w,orld" == ["hello","","w","orld"]
    ]

main :: IO ()
main = do
    testLetterScore
    testWordScore
    testTriangles
    testRuns
    testNumTriangleWords
    testExtractWords
