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

test :: IO ()
test = checkSumDigits

main :: IO ()
main = print (sumDigits (2^1000))
