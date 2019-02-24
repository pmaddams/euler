-- 44. Pentagon numbers

module Main where

import Control.Monad

main :: IO ()
main = print $
    let ds = do
            k <- pentagonals
            j <- takeWhile (< k) pentagonals
            let d = k - j
                s = k + j
            guard (all pentagonal [d,s])
            return d
    in (head ds)

pentagonals :: Integral a => [a]
pentagonals = scanl1 (+) (iterate (+3) 1)

pentagonal :: Integral a => a -> Bool
pentagonal n = any ((== 5) . (`rem` 6)) (unsquare (24*n + 1))

unsquare :: Integral a => a -> Maybe a
unsquare n =
     let (i, f) = properFraction (sqrt (fromIntegral n))
     in case f of
            0 -> if i < 0 then Nothing else Just i
            _ -> Nothing
