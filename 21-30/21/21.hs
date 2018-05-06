module Main where

import Control.Exception
import Data.List
import qualified Data.Map as M

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

factors :: Integral a => a -> [a]
factors n = f' n primes
  where
    f' n (p:ps)
        | n <= 1         = []
        | p^2 > n        = [n]
        | n `mod` p == 0 = p : f' (n `div` p) (p:ps)
        | otherwise      = f' n ps

checkFactors :: IO ()
checkFactors = check "factors"
    [ factors 13195 == [5,7,13,29]
    , factors 216 == [2,2,2,3,3,3]
    ]

runs :: Eq a => [a] -> [(a, Int)]
runs = map (\xs -> (head xs, length xs)) . group

checkRuns :: IO ()
checkRuns = check "runs"
    [ runs [1,2,2,3,3,3] == [(1,1),(2,2),(3,3)]
    , runs [1,1,2,2,2,3] == [(1,2),(2,3),(3,1)]
    ]

divisors :: Integral a => a -> [a]
divisors n = sort $ d' (runs (factors n)) [1]
  where
    d' [] acc            = acc
    d' ((p, len):rs) acc = d' rs acc'
      where
        acc' = acc ++ [d * p^x | x <- [1..len], d <- acc]

checkDivisors :: IO ()
checkDivisors = check "divisors"
    [ divisors 12 == [1,2,3,4,6,12]
    , divisors 16 == [1,2,4,8,16]
    ]

sumPropDiv :: Integral a => a -> a
sumPropDiv = sum . init . divisors

checkSumPropDiv :: IO ()
checkSumPropDiv = check "sumPropDiv"
    [ sumPropDiv 220 == 284
    , sumPropDiv 284 == 220
    ]

amicables :: Integral a => [a]
amicables = a' [2..]
  where
    a' (n:ns) =
        let n' = sumPropDiv n
        in if n /= n' && sumPropDiv n' == n
           then n : a' ns
           else a' ns

checkAmicables :: IO ()
checkAmicables = check "amicables"
    [ take 3 amicables == [220,284,1184]
    ]

test :: IO ()
test = do
    checkPrimes
    checkFactors
    checkRuns
    checkDivisors
    checkSumPropDiv
    checkAmicables

main :: IO ()
main = print (sum (takeWhile (< 10000) amicables))
