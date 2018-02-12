import Control.Exception

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

test :: IO ()
test = checkFibonacci

main :: IO ()
main = print (sum (filter even (takeWhile (< 4000000) fibonacci)))
