import Control.Exception

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

pentagons :: [Int]
pentagons = [(n*(3*n-1)) `div` 2 | n <- [1..]]

checkPentagons :: IO ()
checkPentagons = check "pentagons"
    [ take 10 pentagons == [1,5,12,22,35,51,70,92,117,145]
    ]

perfectSquare :: Int -> Bool
perfectSquare n =
    let m = floor (sqrt (fromIntegral n))
    in m^2 == n

checkPerfectSquare :: IO ()
checkPerfectSquare = check "perfectSquare"
    [ all perfectSquare (map (^2) [1..5])
    , not (any perfectSquare (map ((+1) . (^2)) [1..5]))
    ]

pentagonal :: Int -> Bool
pentagonal n =
    let m = 24*n + 1
    in perfectSquare m &&
       floor (sqrt (fromIntegral m)) `mod` 6 == 5

checkPentagonal :: IO ()
checkPentagonal = check "pentagonal"
    [ all pentagonal (take 10 pentagons)
    , not (pentagonal 48)
    ]

test :: IO ()
test = do
    checkPentagons
    checkPerfectSquare
    checkPentagonal

main :: IO ()
main = print $ head $
    [ (k-j)
    | k <- pentagons
    , j <- takeWhile (< k) pentagons
    , pentagonal (k-j)
    , pentagonal (j+k)
    ]
