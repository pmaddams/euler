module Test where

import Main hiding (main)

main :: IO ()
main = do
    testTriplets
    testIsqrt

testTriplets :: IO ()
testTriplets = test "triplets"
    [ all (`elem` triplets) [[3,4,5], [6,8,10], [5,12,13]]
    , all (\(a:b:c:_) -> a^2 + b^2 == c^2) (take 10 triplets)
    ]

testIsqrt :: IO ()
testIsqrt = test "isqrt"
    [ let ns = [1..10] in map (isqrt . (^2)) ns == ns
    , all (\n -> (isqrt n)^2 <= n) [1..10]
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
