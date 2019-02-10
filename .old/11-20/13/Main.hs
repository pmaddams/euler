module Main where

import Data.Char

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral

fromDigits :: Integral a => [Int] -> a
fromDigits = fromIntegral . read . concatMap show

main :: IO ()
main = do
    s <- readFile "13.txt"
    ss <- return (lines s)
    ns <- return (map read ss)
    let ds = toDigits (sum ns)
    print (fromDigits (take 10 ds))
