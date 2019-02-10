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

testMultiplyPosition :: IO ()
testMultiplyPosition = test "multiplyPosition"
    [ multiplyPosition [4,5,6] == [4,10,18]
    , multiplyPosition [7,8,9,10] == [7,16,27,40]
    ]

testExtractWords :: IO ()
testExtractWords = test "extractWords"
    [ extractWords "hello, world" == ["hello","world"]
    , extractWords "hello,,w,orld" == ["hello","","w","orld"]
    ]

main :: IO ()
main = do
    testLetterScore
    testMultiplyPosition
    testExtractWords
