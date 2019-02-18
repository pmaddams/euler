module Test where

import Data.Maybe

import Main hiding (main)

main :: IO ()
main = do
    testTriplets
    testUnsquare

testTriplets :: IO ()
testTriplets = test "triplets"
    [ all (`elem` triplets) [[3,4,5], [6,8,10], [5,12,13]]
    , all (\[a,b,c] -> a^2 + b^2 == c^2) (take 10 triplets)
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
