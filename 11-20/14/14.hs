module Main where

import Control.Exception
import Data.Array
import Data.List
import Data.Ord

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

collatz :: Int -> [Int]
collatz n
    | n < 1  = []
    | n == 1 = [1]
    | even n = n : collatz (n `div` 2)
    | odd n  = n : collatz (3*n + 1)

checkCollatz :: IO ()
checkCollatz = check "collatz"
    [ collatz 13 == [13,40,20,10,5,16,8,4,2,1]
    , collatz 5 == [5,16,8,4,2,1]
    ]

lengthsUpTo :: Int -> [(Int, Int)]
lengthsUpTo n = assocs arr
  where
    arr = listArray (1, n) (1 : map l' [2..n])

    l' ix = l'' ix (collatz ix)

    l'' _ []        = 0
    l'' ix (c:cs)
        | c < ix    = arr ! c
        | otherwise = 1 + l'' ix cs

checkLengthsUpTo :: IO ()
checkLengthsUpTo = check "lengthsUpTo"
    [ map snd (lengthsUpTo 10) == map (length . collatz) [1..10]
    , snd (last (lengthsUpTo 100)) == length (collatz 100)
    ]

test :: IO ()
test = do
    checkCollatz
    checkLengthsUpTo

main :: IO ()
main = print (fst (maximumBy (comparing snd) (lengthsUpTo 1000000)))
