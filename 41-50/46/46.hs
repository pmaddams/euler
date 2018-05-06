module Main where

check :: String -> [Bool] -> IO ()
check name tests =
    if not (and tests)
    then error name
    else return ()

modExp :: Integer -> Integer -> Integer -> Integer
modExp _ _ 1 = 0
modExp b x m = m' (b `mod` m) x
  where
    m' _ 0 = 1
    m' b x =
        let b' = (b^2) `mod` m
            x' = x `div` 2
        in if x `mod` 2 == 1
           then (b * m' b' x') `mod` m
           else m' b' x'

checkModExp :: IO ()
checkModExp = check "modExp"
    [ modExp 5 3 3 == 2
    , modExp 2 3 5 == 3
    ]

millerRabin :: Integer -> [Integer] -> Bool
millerRabin n []     = True
millerRabin n (a:as) =
    modExp a (n-1) n == 1 &&
    millerRabin n as

prime :: Integral a => a -> Bool
prime n = n > 1 &&
    let ps = [2,3,5,7]
    in n `elem` ps ||
       all (\p -> n `mod` p /= 0) ps &&
       millerRabin (toInteger n) (map toInteger ps)

checkPrime :: IO ()
checkPrime = check "prime"
    [ all prime [2,3,5,7,11,13]
    , not (any prime [-1,0,1,4,9])
    ]

sumOfPrimeAndDoubleSquare :: Int -> Bool
sumOfPrimeAndDoubleSquare n =
    let doubleSquares = takeWhile (< n) (map ((*2) . (^2)) [1..])
    in s' n doubleSquares
  where
    s' _ []     = False
    s' n (d:ds) =
       if prime (n-d)
       then True
       else s' n ds

checkSumOfPrimeAndDoubleSquare :: IO ()
checkSumOfPrimeAndDoubleSquare = check "sumOfPrimeAndDoubleSquare"
    [ all sumOfPrimeAndDoubleSquare [9,15,21,25,27,33]
    , not (any sumOfPrimeAndDoubleSquare [1,2,3,6,8,12])
    ]

test :: IO ()
test = do
    checkModExp
    checkPrime
    checkSumOfPrimeAndDoubleSquare

main :: IO ()
main = print $ head $
    [ n
    | n <- [3,5..]
    , not (prime n)
    , not (sumOfPrimeAndDoubleSquare n)
    ]
