module Test where

import Main hiding (main)

main :: IO ()
main = testLexPermutations

testLexPermutations :: IO ()
testLexPermutations = test "lexPermutations"
    [ lexPermutations [1,2] == [[1,2],[2,1]]
    , lexPermutations "123" == ["123","132","213","231","312","321"]
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
