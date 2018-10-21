module Main where

import Data.List
import Data.Ord

remainders :: Int -> [Int]
remainders d = iterate (\r -> (10*r) `rem` d) 1

cycleLength :: Int -> Int
cycleLength d = c' (remainders d) []
  where
    c' (0:rs) _    = 0
    c' (r:rs) seen = case (elemIndex r seen) of
        Nothing -> c' rs (r:seen)
        Just i  -> (i+1)

main :: IO ()
main =
    let pairs = [(n, cycleLength n) | n <- [1..1000]]
    in print (fst (maximumBy (comparing snd) pairs))
