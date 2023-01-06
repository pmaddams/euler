-- 31. Coin sums

module Main where

main :: IO ()
main = print $
    let coins = [1,2,5,10,20,50,100,200]
    in waysToMakeChange 200 coins

waysToMakeChange :: Integral a => a -> [a] -> a
waysToMakeChange _ []     = 0
waysToMakeChange n cs@(c:cs')
    | n < 0     = 0
    | n == 0    = 1
    | otherwise = waysToMakeChange (n-c) cs + waysToMakeChange n cs'
