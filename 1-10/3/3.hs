import Control.Exception
import qualified Data.IntMap as Map

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

primes :: [Int]
primes = sieve [2..] Map.empty
  where
    sieve (x:xs) m = case Map.lookup x m of
        Nothing -> x : sieve xs (Map.insert (x^2) [x] m)
        Just ps -> sieve xs (foldl mark (Map.delete x m) ps)
      where
        mark m p = Map.insertWith (++) (x+p) [p] m

checkPrimes = check "primes"
    [ take 10 primes == [2,3,5,7,11,13,17,19,23,29]
    , last (take 1000 primes) == 7919
    ]

factors :: Int -> [Int]
factors n = f' n primes
  where
    f' n (p:ps)
        | n <= 1         = []
        | p^2 > n        = [n]
        | n `mod` p == 0 = p : f' (n `div` p) (p:ps)
        | otherwise      = f' n ps

checkFactors = check "factors"
    [ factors 13195 == [5,7,13,29]
    , factors 216 == [2,2,2,3,3,3]
    ]

test :: IO ()
test = do
    checkPrimes
    checkFactors

main :: IO ()
main = print (last (factors 600851475143))
