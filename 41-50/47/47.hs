module Main where

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

firstGroupDistinctFactors :: Int -> Int -> Int
firstGroupDistinctFactors n len =
    let ns = map (length . nub . factors) [1..]
        rs = takeWhile (/= (n, len)) (runs ns)
        ix = sum (map snd rs)
    in ix + 1

checkFirstGroupDistinctFactors :: IO ()
checkFirstGroupDistinctFactors = check "firstGroupDistinctFactors"
    [ firstGroupDistinctFactors 2 2 == 14
    , firstGroupDistinctFactors 3 3 == 644
    ]

test :: IO ()
test = do
    checkPrimes
    checkFactors
    checkRuns
    checkFirstGroupDistinctFactors

main :: IO ()
main = print (firstGroupDistinctFactors 4 4)
