import Control.Exception
import Data.Char

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral

checkToDigits :: IO ()
checkToDigits = check "toDigits"
    [ toDigits 12345 == [1..5]
    , toDigits 987654321 == [9,8..1]
    ]

champernowneDigits :: [Int]
champernowneDigits = concatMap toDigits [1..]

checkChampernowneDigits :: IO ()
checkChampernowneDigits = check "champernowneDigits"
    [ take 9 champernowneDigits == [1..9]
    , champernowneDigits !! 11 == 1
    ]

test :: IO ()
test = do
    checkToDigits
    checkChampernowneDigits

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
