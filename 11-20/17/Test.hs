module Test where

import Main hiding (main)

main :: IO ()
main = testSay

testSay :: IO ()
testSay = test "say"
    [ say 123 == "One Hundred and Twenty Three"
    , say 54321 == "Fifty Four Thousand, Three Hundred and Twenty One"
    , say (-9182736450) ==
          "Negative Nine Billion, " ++
          "One Hundred and Eighty Two Million, " ++
          "Seven Hundred and Thirty Six Thousand, " ++
          "Four Hundred and Fifty"
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
