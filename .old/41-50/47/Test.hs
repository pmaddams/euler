module Test where

import Main hiding (main)

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name

testPrimes :: IO ()
testPrimes = test "primes"
    [ take 6 primes == [2,3,5,7,11,13]
    , primes !! 999 == 7919
    ]

testFactors :: IO ()
testFactors = test "factors"
    [ factors 13195 == [5,7,13,29]
    , factors 216 == [2,2,2,3,3,3]
    ]

testRuns :: IO ()
testRuns = test "runs"
    [ runs [1,2,2,3,3,3] == [(1,1),(2,2),(3,3)]
    , runs [1,1,2,2,2,3] == [(1,2),(2,3),(3,1)]
    ]

testFirstGroupDistinctFactors :: IO ()
testFirstGroupDistinctFactors = test "firstGroupDistinctFactors"
    [ firstGroupDistinctFactors 2 2 == 14
    , firstGroupDistinctFactors 3 3 == 644
    ]

main :: IO ()
main = do
    testPrimes
    testFactors
    testRuns
    testFirstGroupDistinctFactors
