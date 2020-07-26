-- 9. Special Pythagorean triplet

module Main where

import Control.Monad
import Data.List
import Data.Maybe

main :: IO ()
main = print (product ns)
  where
    ns = fromJust (find ((== 1000) . sum) triplets)

triplets :: Integral a => [[a]]
triplets = do
    t <- [2..]
    s <- [1..t-1]
    let r = unsquare (2*s*t)
    maybe [] (\r -> return [r+s,r+t,r+s+t]) r

unsquare :: Integral a => a -> Maybe a
unsquare n =
     let (i, f) = properFraction (sqrt (fromIntegral n))
     in case f of
            0 -> if i < 0 then Nothing else Just i
            _ -> Nothing
