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
cycleLength d =
    let rs = iterate (\n -> rem (10*n) d) 1
    in loop rs []
  where
    loop (0:_) _    = 0
    loop (r:rs) acc = case elemIndex r acc of
        Just i  -> i + 1
        Nothing -> loop rs (r:acc)

best :: Ord b => [(a, b)] -> a
best = fst . maximumBy (comparing snd)
