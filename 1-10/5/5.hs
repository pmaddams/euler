import Control.Exception

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

leastMultiple :: [Int] -> Int
leastMultiple = foldr1 lcm

checkLeastMultiple :: IO ()
checkLeastMultiple = check "leastMultiple"
    [ leastMultiple [1..10] == 2520
    , leastMultiple [2,3,4,6] == 12
    ]

test :: IO ()
test = checkLeastMultiple

main :: IO ()
main = print (leastMultiple [1..20])
