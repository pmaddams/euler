module Test where

import Main hiding (main)

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name

testHorProds :: IO ()
testHorProds = test "horProds"
    [ horProds 2 [[1,2,3],[4,5,6],[7,8,9]] == [2,6,20,30,56,72]
    , horProds 3 [[1,2,3],[4,5,6],[7,8,9]] == [6,120,504]
    ]

testVertProds :: IO ()
testVertProds = test "vertProds"
    [ vertProds 2 [[1,2,3],[4,5,6],[7,8,9]] == [4,10,18,28,40,54]
    , vertProds 3 [[1,2,3],[4,5,6],[7,8,9]] == [28,80,162]
    ]

testDiagDownProds :: IO ()
testDiagDownProds = test "diagDownProds"
    [ diagDownProds 2 [[1,2,3],[4,5,6],[7,8,9]] == [5,12,32,45]
    , diagDownProds 3 [[1,2,3],[4,5,6],[7,8,9]] == [45]
    ]

testDiagUpProds :: IO ()
testDiagUpProds = test "diagUpProds"
    [ diagUpProds 2 [[1,2,3],[4,5,6],[7,8,9]] == [8,15,35,48]
    , diagUpProds 3 [[1,2,3],[4,5,6],[7,8,9]] == [105]
    ]

main :: IO ()
main = do
    testHorProds
    testVertProds
    testDiagDownProds
    testDiagUpProds
