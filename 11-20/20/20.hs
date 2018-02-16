import Control.Exception
import Data.Char

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

sumDigits :: Integer -> Int
sumDigits = sum . map digitToInt . show

checkSumDigits :: IO ()
checkSumDigits = check "sumDigits"
    [ sumDigits 12345 == sum [1..5]
    , sumDigits 67890 == sum [6..9]
    ]

factorial :: (Integral a, Integral b) => a -> b
factorial n = product (map fromIntegral [2..n])

checkFactorial :: IO ()
checkFactorial = check "factorial"
    [ and (map ((==1) . factorial) [-1..1])
    , factorial 10 == 3628800
    ]

test :: IO ()
test = do
    checkSumDigits
    checkFactorial

main :: IO ()
main = print (sumDigits (factorial 100))
