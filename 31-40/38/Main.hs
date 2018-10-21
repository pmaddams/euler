module Main where

import Data.Char
import Data.List
import Data.Maybe

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral

fromDigits :: Integral a => [Int] -> a
fromDigits = fromIntegral . read . concatMap show
 
maybePandigitalMultiple :: Int -> Maybe Int
maybePandigitalMultiple n =
    if n < 1
    then Nothing
    else m' 1 []
  where
    m' i acc =
        let acc' = acc ++ toDigits (n*i)
            len = length acc'
        in if len < 9
           then m' (i+1) acc'
           else if len == 9 && sort acc' == [1..9]
           then Just (fromDigits acc')
           else Nothing

main :: IO ()
main =
    let ms = map maybePandigitalMultiple [1..9999]
    in print (maximum (catMaybes ms))
