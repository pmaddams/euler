module Main where

check :: String -> [Bool] -> IO ()
check name tests =
    if and tests
    then return ()
    else error name

diffSumSquares :: [Int] -> Int
diffSumSquares ns = (sum ns)^2 - sum (map (^2) ns)

checkDiffSumSquares :: IO ()
checkDiffSumSquares = check "diffSumSquares"
    [ diffSumSquares [1..10] == 2640
    , diffSumSquares [20,30] == 1200
    ]

test :: IO ()
test = checkDiffSumSquares

main :: IO ()
main = print (diffSumSquares [1..100])
