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

main :: IO ()
main = testPrimes
