-- 48. Self powers

module Main where

main :: IO ()
main = print $
    let ns = [1..]
        ps = zipWith f ns ns
    in rem (sum (take 1000 ps)) m
  where
    m = 10^10
    f a n = expmod a n m

expmod :: Integral a => a -> a -> a -> a
expmod a n m = loop n
  where
    loop 0 = 1
    loop n =
        let n' =
             if even n
             then (loop (quot n 2))^2
             else a * (loop (n-1))
        in rem n' m
