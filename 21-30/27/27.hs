import Control.Exception
import Data.List
import qualified Data.Map as M
import Data.Ord

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

primes :: [Integer]
primes = sieve [2..] M.empty
  where
    sieve (n:ns) m = case M.lookup n m of
        Nothing -> n : sieve ns (M.insert (n^2) [n] m)
        Just ps -> sieve ns (foldl mark (M.delete n m) ps)
      where
        mark m p = M.insertWith (++) (n+p) [p] m

checkPrimes :: IO ()
checkPrimes = check "primes"
    [ take 6 primes == [2,3,5,7,11,13]
    , primes !! 999 == 7919
    ]

millerRabin :: Integer -> [Integer] -> Bool
millerRabin n []     = True
millerRabin n (a:as) =
    a^(n-1) `mod` n == 1 &&
    millerRabin n as

smallPrime :: Integer -> Bool
smallPrime n = n > 1 &&
    let ps = take 3 primes
    in n `elem` ps ||
       all (\p -> n `mod` p /= 0) ps &&
       millerRabin n ps

checkSmallPrime :: IO ()
checkSmallPrime = check "smallPrime"
    [ all smallPrime (take 100 primes)
    , not (any smallPrime [-1,0,1,4,9])
    ]

asAndBs :: [(Integer, Integer)]
asAndBs =
    [ (a, b)
    | b <- takeWhile (< 1000) (dropWhile (< 41) primes)
    , a <- [(2-b)..min (b-40) 1000]
    , let c = 1+a+b in smallPrime c
    , let d = 4+2*a+b in smallPrime d
    ]

f = fst $ maximumBy (comparing snd)
    ([ (a*b, len)
     | (a,b) <- asAndBs
     , let len = length $
               takeWhile smallPrime
                   [ c
                   | n <- [1..1000]
                   , let c = n^2 + a*n + b
                   , c > 1]
     ])

test :: IO ()
test = do
    checkPrimes
    checkSmallPrime

main :: IO ()
main = print f
