module Main where

import Data.Char
import Data.List

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
millerRabin n as = all (\a -> modExp a (n-1) n == 1) as

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

fromDigits :: Integral a => [Int] -> a
fromDigits = fromIntegral . read . concatMap show

checkFromDigits :: IO ()
checkFromDigits = check "fromDigits"
    [ fromDigits [1..5] == 12345
    , fromDigits [9,8..1] == 987654321
    ]

pandigitals :: [Int]
pandigitals =
    let ds = [1..9]
        ls = reverse (take <$> ds <*> [ds])
        ps = concatMap (sortBy (flip compare) . permutations) ls
    in map fromDigits ps

checkPandigitals :: IO ()
checkPandigitals = check "pandigitals"
    [ head pandigitals == 987654321
    , last pandigitals == 1
    ]

test :: IO ()
test = do
    checkModExp
    checkPrime
    checkFromDigits
    checkPandigitals

main :: IO ()
main = case (find prime pandigitals) of
    Just n  -> print n
    Nothing -> putStrLn "none found"
