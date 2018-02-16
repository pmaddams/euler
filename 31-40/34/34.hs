import Control.Exception
import Data.Char

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

toDigits :: Int -> [Int]
toDigits = map digitToInt . show

checkToDigits :: IO ()
checkToDigits = check "toDigits"
    [ toDigits 12345 == [1..5]
    , toDigits 987654321 == [9,8..1]
    ]

sumFactorialDigits :: Int -> Int
sumFactorialDigits = sum . map (\n -> product [2..n]) . toDigits

checkSumFactorialDigits :: IO ()
checkSumFactorialDigits = check "sumFactorialDigits"
    [ sumFactorialDigits 123 == 9
    , sumFactorialDigits 145 == 145
    ]

test :: IO ()
test = do
    checkToDigits
    checkSumFactorialDigits

main :: IO ()
main = print (sum [n | n <- [3..50000], n == sumFactorialDigits n])
