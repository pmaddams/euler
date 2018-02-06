import Control.Exception

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

divisibleBy :: [Int] -> Int
divisibleBy = foldr1 lcm

checkDivisibleBy :: IO ()
checkDivisibleBy = check "divisibleBy"
    [ divisibleBy [1..10] == 2520
    , divisibleBy [2,3,4,6] == 12
    ]

test :: IO ()
test = checkDivisibleBy

main :: IO ()
main = print (divisibleBy [1..20])
