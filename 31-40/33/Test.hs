module Test where

import Main hiding (main)

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name

testToDigits :: IO ()
testToDigits = test "toDigits"
    [ toDigits 12345 == [1..5]
    , toDigits 987654321 == [9,8..1]
    ]

testLowestTerms :: IO ()
testLowestTerms = test "lowestTerms"
    [ lowestTerms (49, 98) == (1, 2)
    , lowestTerms (4, 8) == (1, 2)
    ]

testEquivalent :: IO ()
testEquivalent = test "equivalent"
    [ equivalent (49, 98) (4, 8)
    , not (equivalent (94, 89) (4, 8))
    ]

testCancelling :: IO ()
testCancelling = test "cancelling"
    [ cancelling (49, 98)
    , not (cancelling (30, 50))
    ]

testMultiplyFractions :: IO ()
testMultiplyFractions = test "multiplyFractions"
    [ multiplyFractions (1, 2) (5, 10) == (1, 4)
    , multiplyFractions (3, 4) (2, 3) == (1, 2)
    ]

main :: IO ()
main = do
    testToDigits
    testLowestTerms
    testEquivalent
    testCancelling
    testMultiplyFractions
