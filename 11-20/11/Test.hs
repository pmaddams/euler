module Test where

import Data.List

import Main hiding (main)

grid :: Grid Int
grid = makeGrid [[1,2,3],[4,5,6],[7,8,9]]

main :: IO ()
main = do
    testConnect
    testWalk

testConnect :: IO ()
testConnect = test "connect"
    [ sort (connect 3 grid) ==
          [ [1,2,3]
          , [1,4,7]
          , [1,5,9]
          , [2,5,8]
          , [3,5,7]
          , [3,6,9]
          , [4,5,6]
          , [7,8,9]
          ]
    ]

testWalk :: IO ()
testWalk = test "walk"
    [ walk 2 grid east (1, 2) == [2,3]
    , walk 2 grid southwest (2, 3) == [6,8]
    , walk 3 grid south (1, 1) == [1,4,7]
    , walk 3 grid southeast (2, 2) == []
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
