import Control.Exception

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

countChange :: Int -> [Int] -> Int
countChange _ [] = 0
countChange n (c:cs)
    | n < 0      = 0
    | n == 0     = 1
    | otherwise  = countChange (n-c) (c:cs) + countChange n cs

checkCountChange :: IO ()
checkCountChange = check "countChange"
    [ countChange 5 [1,5,10] == 2
    , countChange 10 [1,5,10] == 4
    ]

test :: IO ()
test = checkCountChange

main :: IO ()
main = print (countChange 200 [1,2,5,10,20,50,100,200])
