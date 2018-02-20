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
    let ps = take 4 primes
    in n `elem` ps ||
       all (\p -> n `mod` p /= 0) ps &&
       millerRabin (toInteger n) (map toInteger ps)

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

primeRotations :: Int -> Bool
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
    checkModExp
    checkSmallPrime
    checkToDigits
    checkFromDigits
    checkRotations
    checkPrimeRotations

main :: IO ()
main =
    let ps = takeWhile (<1000000) primes
    in print (length (filter primeRotations ps))
