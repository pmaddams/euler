import Control.Exception
import qualified Data.Map as M

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

test :: IO ()
test = checkPrimes

main :: IO ()
main = print (primes !! 10000)
