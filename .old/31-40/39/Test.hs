module Test where

import Main hiding (main)

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name

testPrimitiveTriples :: IO ()
testPrimitiveTriples = test "primitiveTriples"
    [ head primitiveTriples == [3, 4, 5]
    , and (map (\t ->
               let a = t !! 0
                   b = t !! 1
                   c = t !! 2
               in a^2+b^2 == c^2)
          (take 10 primitiveTriples))
    ]

testTriplesUpTo :: IO ()
testTriplesUpTo = test "triplesUpTo"
    [ triplesUpTo 24 == [[3,4,5],[6,8,10]]
    , [30,40,50] `elem` (triplesUpTo 120)
    ]

testRuns :: IO ()
testRuns = test "runs"
    [ runs [1,2,2,3,3,3] == [(1,1),(2,2),(3,3)]
    , runs [1,1,2,2,2,3] == [(1,2),(2,3),(3,1)]
    ]

main :: IO ()
main = do
    testPrimitiveTriples
    testTriplesUpTo
    testRuns
