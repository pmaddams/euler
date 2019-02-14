module Test where

import Main hiding (main)

main :: IO ()
main = return ()

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
