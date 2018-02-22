import Control.Exception

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

pentagons :: [Int]
pentagons = [(n*(3*n-1)) `div` 2 | n <- [1..]]

checkPentagons :: IO ()
checkPentagons = check "pentagons"
    [ take 5 pentagons == [1,5,12,22,35]
    , 1001 `elem` pentagons
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
    [ all pentagonal [1,5,12,22,35]
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
