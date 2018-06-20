module Main where

import Data.Char

check :: String -> [Bool] -> IO ()
check name tests =
    if and tests
    then return ()
    else error name

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral

checkToDigits :: IO ()
checkToDigits = check "toDigits"
    [ toDigits 12345 == [1..5]
    , toDigits 987654321 == [9,8..1]
    ]

test :: IO ()
test = checkToDigits

main :: IO ()
main = print (sum (toDigits (2^1000)))
