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

toBits :: Integral a => a -> [Int]
toBits n = b' n []
  where
    b' 0 acc = acc
    b' n acc = b' (n `div` 2) (fromIntegral (n `mod` 2) : acc)

checkToBits :: IO ()
checkToBits = check "toBits"
    [ toBits 4 == [1,0,0]
    , toBits 5 == [1,0,1]
    ]

palindrome :: Eq a => [a] -> Bool
palindrome xs = xs == reverse xs

checkPalindrome :: IO ()
checkPalindrome = check "palindrome"
    [ palindrome [1,2,3,2,1]
    , not (palindrome [1..5])
    ]

doublePalindrome :: Integral a => a -> Bool
doublePalindrome n = odd n &&
    palindrome (toDigits n) &&
    palindrome (toBits n)

checkDoublePalindrome :: IO ()
checkDoublePalindrome = check "doublePalindrome"
    [ not (doublePalindrome 4)
    , doublePalindrome 585
    ]

test :: IO ()
test = do
    checkToDigits
    checkToBits
    checkPalindrome
    checkDoublePalindrome

main :: IO ()
main = print (sum (filter doublePalindrome [1..999999]))
