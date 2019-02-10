module Main where

import Data.Char

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral

champernowneDigits :: [Int]
champernowneDigits = concatMap toDigits [1..]

main :: IO ()
main =
    let ds = [ champernowneDigits !! 0
             , champernowneDigits !! 9
             , champernowneDigits !! 99
             , champernowneDigits !! 999
             , champernowneDigits !! 9999
             , champernowneDigits !! 99999
             , champernowneDigits !! 999999
             ]
    in print (product ds)
