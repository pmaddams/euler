module Test where

import Main hiding (main)

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name

testPowers :: IO ()
testPowers = test "powers"
    [ powers [1..3] [1..3] == [1,2,3,4,8,9,27]
    , powers [2..5] [2..5] ==
          [4,8,9,16,25,27,32,64,81,125,243,256,625,1024,3125]
    ]

main :: IO ()
main = testPowers
