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

factorial :: (Integral a, Integral b) => a -> b
factorial n = product (map fromIntegral [2..n])

checkFactorial :: IO ()
checkFactorial = check "factorial"
    [ and (map ((==1) . factorial) [-1..1])
    , factorial 10 == 3628800
    ]

sumDigitFactorials :: Int -> Int
sumDigitFactorials = sum . (map factorial) . toDigits

checkSumDigitFactorials :: IO ()
checkSumDigitFactorials = check "sumFactorialDigits"
    [ sumDigitFactorials 123 == 9
    , sumDigitFactorials 145 == 145
    ]

test :: IO ()
test = do
    checkToDigits
    checkSumDigitFactorials

main :: IO ()
main = print (sum [n | n <- [3..50000], n == sumDigitFactorials n])
