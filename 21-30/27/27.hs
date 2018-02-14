import Control.Exception
import Data.List
import qualified Data.Map as M
import Data.Ord
import qualified Data.Set as S

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

asAndBs :: [(Integer, Integer)]
asAndBs =
    [ (a, b)
    | b <- takeWhile (< 1000) (dropWhile (< 41) primes)
    , a <- [(2-b)..min (b-40) 1000]
    , let c = 1+a+b in c == head (dropWhile (< c) primes)
    ]

f = let s = S.fromList (takeWhile (< 2000000) primes)
        prime n = S.member n s
    in fst $ maximumBy (comparing snd)
           ([ (a*b, len)
            | (a,b) <- asAndBs
            , let len = length (takeWhile prime [n^2 + a*n + b | n <- [1..1000]])
            ])

test :: IO ()
test = do
    checkPrimes

main :: IO ()
main = print f
