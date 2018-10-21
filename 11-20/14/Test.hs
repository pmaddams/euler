module Test where

import Main hiding (main)

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name

testCollatz :: IO ()
testCollatz = test "collatz"
    [ collatz 13 == [13,40,20,10,5,16,8,4,2,1]
    , collatz 5 == [5,16,8,4,2,1]
    ]

testLengthsUpTo :: IO ()
testLengthsUpTo = test "lengthsUpTo"
    [ map snd (lengthsUpTo 10) == map (length . collatz) [1..10]
    , snd (last (lengthsUpTo 100)) == length (collatz 100)
    ]

main :: IO ()
main = do
    testCollatz
    testLengthsUpTo
