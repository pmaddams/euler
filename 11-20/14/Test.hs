module Test where

import Main hiding (main)

main :: IO ()
main = do
    testCollatzLengthsUpTo
    testCollatz
    testTakeUntil
    testBest

testCollatzLengthsUpTo :: IO ()
testCollatzLengthsUpTo = test "collatzLengthsUpTo"
    [ map snd (collatzLengthsUpTo 100) == map (length . collatz) [1..100]
    ]

testCollatz :: IO ()
testCollatz = test "collatz"
    [ collatz 1 == [1]
    , collatz 13 == [13,40,20,10,5,16,8,4,2,1]
    ]

testTakeUntil :: IO ()
testTakeUntil = test "takeUntil"
    [ takeUntil (> 3) [1..] == [1..4]
    , takeUntil (\x -> x / 0 == 1) [] == []
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
