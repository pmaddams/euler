-- 8. Largest product in a series

module Main where

import Data.Char

main :: IO ()
main = do
    ds <- readDigits "8.txt"
    let ns = map product (adjacent 13 ds)
    print (maximum ns)

adjacent :: Int -> [a] -> [[a]]
adjacent n xs@(_:xs') =
    let ys = take n xs
    in if length ys < n
       then []
       else ys : adjacent n xs'

readDigits :: FilePath -> IO [Int]
readDigits name = readFile name >>= f
  where
    f = return . map digitToInt . filter isDigit
