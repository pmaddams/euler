module Main where

modExp :: Integer -> Integer -> Integer -> Integer
modExp _ _ 1 = 0
modExp b x m = m' (b `mod` m) x
  where
    m' _ 0 = 1
    m' b x =
        let b' = (b^2) `mod` m
            x' = x `div` 2
        in if x `mod` 2 == 1
           then (b * m' b' x') `mod` m
           else m' b' x'

main :: IO ()
main =
    let ns = [1..1000]
        m = 10^10
        rs = zipWith3 modExp ns ns (repeat m)
    in print (sum rs `mod` m)
