module Main where

import Control.Exception
import Data.List

check :: String -> [Bool] -> IO ()
check name tests =
    if not (and tests)
    then error name
    else return ()

lexPerms :: [Int] -> [String]
lexPerms = sort . permutations . concatMap show

checkLexPerms :: IO ()
checkLexPerms = check "lexPerms"
    [ "3124" `elem` (lexPerms [1..4])
    , lexPerms [0..2] == ["012","021","102","120","201","210"]
    ]

test :: IO ()
test = checkLexPerms

main :: IO ()
main = print ((lexPerms [0..9]) !! 999999)
