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

factorial :: (Integral a, Integral b) => a -> b
factorial n = product (map fromIntegral [2..n])

checkFactorial :: IO ()
checkFactorial = check "factorial"
    [ and (map ((==1) . factorial) [-1..1])
    , factorial 10 == 3628800
    ]

test :: IO ()
test = do
    checkToDigits
    checkFactorial

main :: IO ()
main = print (sum (toDigits (factorial 100)))
