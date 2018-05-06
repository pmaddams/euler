module Main where

import Control.Exception

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

onesWord :: Int -> String
onesWord n = case n of
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"

teenWord :: Int -> String
teenWord n = case n of
    10 -> "ten"
    11 -> "eleven"
    12 -> "twelve"
    13 -> "thirteen"
    14 -> "fourteen"
    15 -> "fifteen"
    16 -> "sixteen"
    17 -> "seventeen"
    18 -> "eighteen"
    19 -> "nineteen"

tensWord :: Int -> String
tensWord n = case n of
    20 -> "twenty"
    30 -> "thirty"
    40 -> "forty"
    50 -> "fifty"
    60 -> "sixty"
    70 -> "seventy"
    80 -> "eighty"
    90 -> "ninety"

toWords :: Int -> String
toWords n
    | n >= 1000 = "onethousand"
    | n >= 100  = let r = n `mod` 100
                      d = n `div` 100
                  in if r == 0
                  then onesWord d ++ "hundred"
                  else onesWord d ++ "hundredand" ++ toWords r
    | n >= 20   = let r = n `mod` 10
                  in tensWord (n-r) ++ toWords r
    | n >= 10   = teenWord n
    | n > 0     = onesWord n
    | otherwise = ""

checkToWords :: IO ()
checkToWords = check "toWords"
    [ toWords 100 == "onehundred"
    , toWords 115 == "onehundredandfifteen"
    , toWords 342 == "threehundredandfortytwo"
    ]

test :: IO ()
test = checkToWords

main :: IO ()
main = print (sum (map (length . toWords) [1..1000]))
