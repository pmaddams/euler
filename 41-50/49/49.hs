module Main where

import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Maybe

check :: String -> [Bool] -> IO ()
check name tests =
    if not (and tests)
    then error name
    else return ()

primes :: Integral a => [a]
primes = sieve [2..] M.empty
  where
    sieve (n:ns) m = case M.lookup n m of
        Just ps -> sieve ns (foldl mark (M.delete n m) ps)
        Nothing -> n : sieve ns (M.insert (n^2) [n] m)
      where
        mark m p = M.insertWith (++) (n+p) [p] m

checkPrimes :: IO ()
checkPrimes = check "primes"
    [ take 6 primes == [2,3,5,7,11,13]
    , primes !! 999 == 7919
    ]

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

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral

checkToDigits :: IO ()
checkToDigits = check "toDigits"
    [ toDigits 12345 == [1..5]
    , toDigits 987654321 == [9,8..1]
    ]

fromDigits :: Integral a => [Int] -> a
fromDigits = fromIntegral . read . concatMap show

checkFromDigits :: IO ()
checkFromDigits = check "fromDigits"
    [ fromDigits [1..5] == 12345
    , fromDigits [9,8..1] == 987654321
    ]

uniqueDigits :: [Int] -> [Int]
uniqueDigits ns = nub $ sort $
    map (fromDigits . sort . toDigits) ns

checkUniqueDigits :: IO ()
checkUniqueDigits = check "uniqueDigits"
    [ uniqueDigits [123,132,213,231,312,321] == [123]
    , uniqueDigits [123,231,456,465,789,987] == [123,456,789]
    ]

primePermutations :: Int -> [Int]
primePermutations n =
    let ds = permutations (toDigits n)
        ns = nub (sort (map fromDigits ds))
    in filter smallPrime ns

digitPermutationOf :: Int -> Int -> Bool
n `digitPermutationOf` m =
    let d' = sort . toDigits
    in d' n == d' m

checkDigitPermutationOf :: IO ()
checkDigitPermutationOf = check "digitPermutationOf"
    [ 123 `digitPermutationOf` 321
    , not (123 `digitPermutationOf` 311)
    ]

primePermutationSequence :: Int -> Maybe (Int, Int, Int)
primePermutationSequence = p' . primePermutations
  where
    p' []     = Nothing
    p' (n:ns) =
        let ps = p'' n ns
        in if (isNothing ps)
           then p' ns
           else ps

    p'' n []       = Nothing
    p'' n (n':ns') =
        let n'' = 2*n' - n
        in if smallPrime n'' &&
              n'' `digitPermutationOf` n
           then Just (n, n', n'')
           else p'' n ns'

findPrimePermutationSequence :: [Int] -> Maybe (Int, Int, Int)
findPrimePermutationSequence []     = Nothing
findPrimePermutationSequence (n:ns) =
    let ps = primePermutationSequence n
    in if (isNothing ps)
       then findPrimePermutationSequence ns
       else ps

test :: IO ()
test = do
    checkPrimes
    checkModExp
    checkSmallPrime
    checkToDigits
    checkFromDigits
    checkUniqueDigits
    checkDigitPermutationOf

main :: IO ()
main =
    let ps = dropWhile (<= 1487) (uniqueDigits (takeWhile (< 10000) primes))
    in case (findPrimePermutationSequence ps) of
        Just (n, n', n'') -> print (fromDigits (concatMap toDigits [n, n', n'']))
