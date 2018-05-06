module Main where

import Control.Exception
import Data.Char
import Data.List

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral

checkToDigits :: IO ()
checkToDigits = check "toDigits"
    [ toDigits 12345 == [1..5]
    , toDigits 987654321 == [9,8..1]
    ]

fromDigits :: Integral a => [Int] -> a
fromDigits = fromIntegral . read . concatMap show
 
checkFromDigits :: IO ()
checkFromDigits = check "fromDigits"
    [ fromDigits [1..5] == 12345
    , fromDigits [9,8..1] == 987654321
    ]

pandigitalProducts :: [(Int, Int, Int)]
pandigitalProducts =
    [ (m1, m2, p)
    | ns <- permutations [1..9]
    , let (lo, hi) = splitAt 5 ns
    , i <- [1, 2]
    , let (ll, lh) = splitAt i lo
          m1 = fromDigits ll
          m2 = fromDigits lh
          p = m1 * m2
    , sort (toDigits p) == hi
    ]

test :: IO ()
test = do
    checkToDigits
    checkFromDigits

main :: IO ()
main = print (sum (nub [p | (_,_,p) <- pandigitalProducts]))
