-- 45. Triangular, pentagonal, and hexagonal

module Main where

import Data.List
import Data.Maybe

main :: IO ()
main = print (fromJust (find pentagonal ns))
  where
    ns = dropWhile (<= 40755) hexagonals

hexagonals :: Integral a => [a]
hexagonals = scanl1 (+) (iterate (+4) 1)

pentagonal :: Integral a => a -> Bool
pentagonal n = any ((== 5) . (`rem` 6)) (unsquare (24*n + 1))

unsquare :: Integral a => a -> Maybe a
unsquare n =
     let (i, f) = properFraction (sqrt (fromIntegral n))
     in case f of
            0 -> if i < 0 then Nothing else Just i
            _ -> Nothing
