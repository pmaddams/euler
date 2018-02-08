import Control.Exception
import Data.List
import qualified Data.Map as M

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

triangles :: [Integer]
triangles = scanl1 (+) [1..]

checkTriangles :: IO ()
checkTriangles = check "triangles"
    [ take 10 triangles == [1,3,6,10,15,21,28,36,45,55]
    , triangles !! 99 == sum [1..100]
    ]

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

factors :: Integer -> [Integer]
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

divisors :: Integer -> Int
divisors n = product $ map ((+1) . length) (group (factors n))

checkDivisors :: IO ()
checkDivisors = check "divisors"
    [ divisors 12 == length [1,2,3,4,6,12]
    , divisors 16 == length [1,2,4,8,16]
    ]

test :: IO ()
test = do
    checkTriangles
    checkPrimes
    checkFactors
    checkDivisors

main :: IO ()
main = case (find ((> 500) . divisors) triangles) of
    Just n -> print n
