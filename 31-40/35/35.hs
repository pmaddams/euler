import Control.Exception
import Data.Char
import qualified Data.Map as M

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

primes :: Integral a => [a]
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
    let ps = take 4 primes
    in n `elem` ps ||
       all (\p -> n `mod` p /= 0) ps &&
       millerRabin n ps

checkSmallPrime :: IO ()
checkSmallPrime = check "smallPrime"
    [ all smallPrime (take 100 primes)
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

rotations :: [a] -> [[a]]
rotations xs = r' xs []
  where
    r' [] _        = []
    r'  (x:xs) acc =
        let acc' = acc ++ [x]
        in (xs ++ acc') : r' xs acc'

checkRotations :: IO ()
checkRotations = check "rotations"
    [ rotations [1,2,3] ==
        [[2,3,1],[3,1,2],[1,2,3]]
    , rotations "hello" ==
        ["elloh","llohe","lohel","ohell","hello"]
    ]

primeRotations :: Integer -> Bool
primeRotations n =
    let ds = toDigits n
    in length ds == 1 ||
       not (any (\d -> d `elem` ds) [0,2,4,5,6,8]) &&
       and (map (smallPrime . fromDigits) (rotations ds))

checkPrimeRotations :: IO ()
checkPrimeRotations = check "primeRotations"
    [ primeRotations 197
    , not (primeRotations 101)
    ]

test :: IO ()
test = do
    checkPrimes
    checkSmallPrime
    checkToDigits
    checkFromDigits
    checkRotations
    checkPrimeRotations

main :: IO ()
main =
    let ps = takeWhile (<1000000) primes
    in print (length (filter primeRotations ps))
