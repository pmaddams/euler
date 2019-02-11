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
    let q = 2*s*t
        r = isqrt q
    guard (r^2 == q)
    return [r+s, r+t, r+s+t]

isqrt :: Integral a => a -> a
isqrt 0 = 0
isqrt n =
    let r1 = 2 * isqrt (quot n 4)
        r2 = r1 + 1
    in if r2^2 > n
       then r1
       else r2
