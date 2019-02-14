-- 31. Coin sums

module Main where

main :: IO ()
main = print $
    let coins = [1,2,5,10,20,50,100,200]
    in change 200 coins

change :: Integral a => a -> [a] -> a
change _ []     = 0
change n cs@(c:cs')
    | n < 0     = 0
    | n == 0    = 1
    | otherwise = change (n-c) cs + change n cs'
