-- 26. Reciprocal cycles

module Main where

import Data.List
import Data.Ord

main :: IO ()
main = print $
    let ds = [1..999]
        ls = map cycleLength ds
    in best (zip ds ls)

cycleLength :: Integral a => a -> Int
cycleLength d = loop [] (iterate (\n -> (10*n) `rem` d) 1)
  where
    loop _ (0:_)    = 0
    loop acc (r:rs) = case elemIndex r acc of
        Just i  -> i + 1
        Nothing -> loop (r:acc) rs

best :: Ord b => [(a, b)] -> a
best = fst . maximumBy (comparing snd)
