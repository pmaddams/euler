module Test where

import Data.Maybe

import Main hiding (main)

main :: IO ()
main = do
    testPentagonals
    testPentagonal
    testUnsquare

testPentagonals :: IO ()
testPentagonals = test "pentagonals"
    [ take 10 pentagonals == [1,5,12,22,35,51,70,92,117,145]
    ]

testPentagonal :: IO ()
testPentagonal = test "pentagonal"
    [ all pentagonal [1,5,12,22,35,51,70,92,117,145]
    , not (any pentagonal ([2..4] ++ [6..11] ++ [13..21]))
    ]

testUnsquare :: IO ()
testUnsquare = test "unsquare"
    [ let ns = [0..10] in map (fromJust . unsquare . (^2)) ns == ns
    , all isNothing (map unsquare ([-1,2,3,5,6,7,8,10]))
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
