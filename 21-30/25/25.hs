import Control.Exception
import Data.List

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

fibonacci :: [Integer]
fibonacci = 1 : 1 : zipWith (+) fibonacci (tail fibonacci)
  
checkFibonacci :: IO ()
checkFibonacci = check "fibonacci"
    [ take 10 fibonacci == [1,1,2,3,5,8,13,21,34,55]
    , last (takeWhile (< 4000000) fibonacci) == 3524578
    ]

numDigits :: Integer -> Int
numDigits = length . show

checkNumDigits :: IO ()
checkNumDigits = check "numDigits"
    [ numDigits 12345 == 5
    , numDigits 1234567890 == 10
    ]

test :: IO ()
test = do
    checkFibonacci
    checkNumDigits

main :: IO ()
main = case (findIndex ((>= 1000) . numDigits)) fibonacci of
    Just i -> print (i+1)
