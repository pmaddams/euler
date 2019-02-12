-- 13. Large sum

module Main where

import Data.Char

main :: IO ()
main = do
    ns <- readNumbers "13.txt"
    let ds = take 10 (toDigits (sum ns))
    print (fromDigits ds)

toDigits :: (Integral a, Show a) => a -> [Int]
toDigits = map digitToInt . show

fromDigits :: (Integral a, Read a) => [Int] -> a
fromDigits = read . map intToDigit

readNumbers :: (Integral a, Read a) => FilePath -> IO [a]
readNumbers name = readFile name >>= f
  where
    f = return . map read . words
