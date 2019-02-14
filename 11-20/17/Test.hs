module Test where

import Main hiding (main)

main :: IO ()
main = testSay

testSay :: IO ()
testSay = test "say"
    [ say 123 == "One Hundred and Twenty Three"
    , say 54321 == "Fifty Four Thousand, Three Hundred and Twenty One"
    , say 918273645 ==
          "Nine Hundred and Eighteen Million, " ++
          "Two Hundred and Seventy Three Thousand, " ++
          "Six Hundred and Forty Five"
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
