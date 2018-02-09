import Control.Exception

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

choose :: Integer -> Integer -> Integer
n `choose` k = (product [k+1..n]) `div` (product [1..n-k])

checkChoose :: IO ()
checkChoose = check "choose"
    [ 4 `choose` 2 == 6
    , 5 `choose` 3 == 10
    ]

paths :: Integer -> Integer
paths n = (2*n) `choose` n

checkPaths :: IO ()
checkPaths = check "paths"
    [ paths 1 == 2
    , paths 2 == 6
    ]

test :: IO ()
test = do
    checkChoose
    checkPaths

main :: IO ()
main = print (paths 20)
