-- 13. Large sum

module Main where

import Data.Char

main :: IO ()
main = do
    ns <- readNumbers "13.txt"
    let ds = take 10 (toDigits (sum ns))
    print (fromDigits ds)

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral

fromDigits :: Integral a => [Int] -> a
fromDigits = fromIntegral . read . map intToDigit

readNumbers :: (Integral a, Read a) => FilePath -> IO [a]
readNumbers name = readFile name >>= f
  where
    f = return . map read . words
