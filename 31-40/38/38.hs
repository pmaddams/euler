module Main where

import Data.Char
import Data.List
import Data.Maybe

check :: String -> [Bool] -> IO ()
check name tests =
    if not (and tests)
    then error name
    else return ()

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral

checkToDigits :: IO ()
checkToDigits = check "toDigits"
    [ toDigits 12345 == [1..5]
    , toDigits 987654321 == [9,8..1]
    ]

fromDigits :: Integral a => [Int] -> a
fromDigits = fromIntegral . read . concatMap show
 
checkFromDigits :: IO ()
checkFromDigits = check "fromDigits"
    [ fromDigits [1..5] == 12345
    , fromDigits [9,8..1] == 987654321
    ]

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

checkMaybePandigitalMultiple :: IO ()
checkMaybePandigitalMultiple = check "maybePandigitalMultiple"
    [ maybePandigitalMultiple 192 == Just 192384576
    , maybePandigitalMultiple 9 == Just 918273645
    ]

test :: IO ()
test = do
    checkToDigits
    checkFromDigits
    checkMaybePandigitalMultiple

main :: IO ()
main =
    let ms = map maybePandigitalMultiple [1..9999]
    in print (maximum (catMaybes ms))
