module Main where

countChange :: Int -> [Int] -> Int
countChange _ [] = 0
countChange n (c:cs)
    | n < 0      = 0
    | n == 0     = 1
    | otherwise  = countChange (n-c) (c:cs) + countChange n cs

main :: IO ()
main = print (countChange 200 [1,2,5,10,20,50,100,200])
