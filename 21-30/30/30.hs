module Main where

import Data.Char

check :: String -> [Bool] -> IO ()
check name tests =
    if not (and tests)
    then error name
    else return ()

isSumOfPowers :: Int -> Int -> Bool
isSumOfPowers x n = s' (map digitToInt (show n)) 0
  where
    s' [] acc     = n == acc
    s' (d:ds) acc =
        if acc > n 
        then False
        else s' ds (d^x + acc)

checkIsSumOfPowers :: IO ()
checkIsSumOfPowers = check "isSumOfPowers"
    [ not (isSumOfPowers 4 1234)
    , and (map (isSumOfPowers 4) [1634,8208,9474])
    ]

test :: IO ()
test = checkIsSumOfPowers

main :: IO ()
main = print (sum [n | n <- [2..1000000], isSumOfPowers 5 n])
