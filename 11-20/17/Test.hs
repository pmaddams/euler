module Test where

import Main hiding (main)

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name

testToWords :: IO ()
testToWords = test "toWords"
    [ toWords 100 == "onehundred"
    , toWords 115 == "onehundredandfifteen"
    , toWords 342 == "threehundredandfortytwo"
    ]

main :: IO ()
main = testToWords
