import Control.Exception

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

divisibleAny :: Integral a => a -> [a] -> Bool
n `divisibleAny` ds = any (\d -> n `mod` d == 0) ds

checkDivisibleAny :: IO ()
checkDivisibleAny = check "divisibleAny"
    [ not (5 `divisibleAny` [2,3])
    , 5 `divisibleAny` [2,3,5]
    ]

test :: IO ()
test = checkDivisibleAny

main :: IO ()
main =
    let multiples = filter (`divisibleAny` [3,5]) [1..]
    in print (sum (takeWhile (< 1000) multiples))
