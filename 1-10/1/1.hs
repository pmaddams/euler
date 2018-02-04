import Control.Exception

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

multiples :: [Int]
multiples = [x | x <- [1..], x `mod` 3 == 0 || x `mod` 5 == 0]

checkMultiples :: IO ()
checkMultiples = check "multiples"
    [ takeWhile (< 10) multiples == [3,5,6,9]
    , takeWhile (< 20) multiples == [3,5,6,9,10,12,15,18]
    ]

test :: IO ()
test = checkMultiples

main :: IO ()
main = print (sum (takeWhile (< 1000) multiples))
