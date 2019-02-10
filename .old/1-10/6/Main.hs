module Main where

diffSumSquares :: [Int] -> Int
diffSumSquares ns = (sum ns)^2 - sum (map (^2) ns)

main :: IO ()
main = print (diffSumSquares [1..100])
