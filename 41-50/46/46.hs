import Control.Exception
import Data.List

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

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

smallPrime :: Int -> Bool
smallPrime n = n > 1 &&
    let ps = [2,3,5,7]
    in n `elem` ps ||
       all (\p -> n `mod` p /= 0) ps &&
       millerRabin (toInteger n) (map toInteger ps)

checkSmallPrime :: IO ()
checkSmallPrime = check "smallPrime"
    [ all smallPrime [2,3,5,7,11,13]
    , not (any smallPrime [-1,0,1,4,9])
    ]

smallOddComposites :: [Int]
smallOddComposites = [n | n <- [3,5..], not (smallPrime n)]

checkSmallOddComposites :: IO ()
checkSmallOddComposites = check "smallOddComposites"
    [ take 6 smallOddComposites == [9,15,21,25,27,33]
    , 111 `elem` smallOddComposites
    ]

doubleSquares :: Integral a => [a]
doubleSquares = [2*n^2 | n <- [1..]]

checkDoubleSquares :: IO ()
checkDoubleSquares = check "doubleSquares"
    [ take 6 doubleSquares == [2,8,18,32,50,72]
    , doubleSquares !! 999 == 2000000
    ]

sumOfPrimeDoubleSquare :: Int -> Bool
sumOfPrimeDoubleSquare n =
    let ds = takeWhile (< n) doubleSquares
    in s' n ds
  where
    s' _ []     = False
    s' n (d:ds) =
       if smallPrime (n-d)
       then True
       else s' n ds

checkSumOfPrimeDoubleSquare :: IO ()
checkSumOfPrimeDoubleSquare = check "sumOfPrimeDoubleSquare"
    [ all sumOfPrimeDoubleSquare [9,15,21,25,27,33]
    , not (any sumOfPrimeDoubleSquare [1,2,3,6,8,12])
    ]

test :: IO ()
test = do
    checkModExp
    checkSmallPrime
    checkSmallOddComposites
    checkDoubleSquares
    checkSumOfPrimeDoubleSquare

main :: IO ()
main = case (find (not . sumOfPrimeDoubleSquare) smallOddComposites) of
    Just n -> print n
