-- 48. Self powers

module Main where

main :: IO ()
main = print $
    let ns = [1..]
        ps = zipWith f ns ns
    in sum (take 1000 ps) `rem` m
  where
    m = 10^10
    f b n = modPow b n m

modPow :: Integral a => a -> a -> a -> a
modPow b n m = loop n
  where
    loop 0 = 1
    loop n = n' `rem` m
      where
        n' = if even n
             then (loop (n `quot` 2))^2
             else b * (loop (n-1))
