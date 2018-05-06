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

sigmaZero :: Integral a => a -> Int
sigmaZero n = product $ map ((+1) . length) (group (factors n))

checkSigmaZero :: IO ()
checkSigmaZero = check "sigmaZero"
    [ sigmaZero 12 == length [1,2,3,4,6,12]
    , sigmaZero 16 == length [1,2,4,8,16]
    ]

triangles :: [Int]
triangles = scanl1 (+) [1..]

checkTriangles :: IO ()
checkTriangles = check "triangles"
    [ take 10 triangles == [1,3,6,10,15,21,28,36,45,55]
    , triangles !! 99 == sum [1..100]
    ]

test :: IO ()
test = do
    checkPrimes
    checkFactors
    checkSigmaZero
    checkTriangles

main :: IO ()
main = case (find ((> 500) . sigmaZero) triangles) of
    Just n -> print n
