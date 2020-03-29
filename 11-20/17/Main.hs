-- 17. Number letter counts

module Main where

import Data.Char

main :: IO ()
main = print (length (filter isAlpha cs))
  where
    cs = concatMap say [1..1000]

say :: Int -> String
say 0 = "Zero"
say n = unwords (toWords n)

toWords :: Int -> [String]
toWords n
    | n < 0     = "Negative" : toWords (-n)
    | n == 0    = []
    | n < 20    = [show ([One .. Nineteen] !! (n - 1))]
    | n < 100   = let (begin, end) = quotRem n 10
                  in [show ([Twenty .. Ninety] !! (begin - 2))] ++ toWords end
    | n < 10^3  = bigWords n Hundred
    | n < 10^6  = bigWords n Thousand
    | n < 10^9  = bigWords n Million
    | n < 10^12 = bigWords n Billion
    | otherwise = error "toWords: too big"

data Quantity
    = One | Two | Three | Four | Five | Six | Seven | Eight | Nine
    | Ten | Eleven | Twelve | Thirteen | Fourteen | Fifteen | Sixteen | Seventeen | Eighteen | Nineteen
    | Twenty | Thirty | Forty | Fifty | Sixty | Seventy | Eighty | Ninety
    | Hundred | Thousand | Million | Billion
  deriving (Enum, Show)

bigWords :: Int -> Quantity -> [String]
bigWords n q =
    let place = case q of
            Hundred  -> 100
            Thousand -> 10^3
            Million  -> 10^6
            Billion  -> 10^9
            _        -> error "bigWords: unrecognized quantity"
        (begin, end) = quotRem n place
        sep = if end == 0 then "" else if end < 100 then " and" else ","
    in toWords begin ++ [show q ++ sep] ++ toWords end
