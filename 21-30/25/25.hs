module Main where

import Data.Char
import Data.List

check :: String -> [Bool] -> IO ()
check name tests =
    if not (and tests)
    then error name
    else return ()

fibonacci :: [Integer]
fibonacci = 1 : 1 : zipWith (+) fibonacci (tail fibonacci)
  
checkFibonacci :: IO ()
checkFibonacci = check "fibonacci"
    [ take 10 fibonacci == [1,1,2,3,5,8,13,21,34,55]
    , last (takeWhile (< 4000000) fibonacci) == 3524578
    ]

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral

checkToDigits :: IO ()
checkToDigits = check "toDigits"
    [ toDigits 12345 == [1..5]
    , toDigits 987654321 == [9,8..1]
    ]

test :: IO ()
test = do
    checkFibonacci
    checkToDigits

main :: IO ()
main = case (findIndex ((>=1000) . length . toDigits)) fibonacci of
    Just i -> print (i+1)
