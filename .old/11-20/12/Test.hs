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

testSigmaZero :: IO ()
testSigmaZero = test "sigmaZero"
    [ sigmaZero 12 == length [1,2,3,4,6,12]
    , sigmaZero 16 == length [1,2,4,8,16]
    ]

testTriangles :: IO ()
testTriangles = test "triangles"
    [ take 10 triangles == [1,3,6,10,15,21,28,36,45,55]
    , triangles !! 99 == sum [1..100]
    ]

main :: IO ()
main = do
    testPrimes
    testFactors
    testSigmaZero
    testTriangles
