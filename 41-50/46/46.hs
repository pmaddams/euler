import Control.Exception
import Data.List
import qualified Data.Map as M

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

oddComposites :: Integral a => [a]
oddComposites = sieve [2..] M.empty
  where
    sieve (n:ns) m = case M.lookup n m of
        Just ps -> let rest = sieve ns (foldl mark (M.delete n m) ps)
                   in if odd n
                      then n : rest
                      else rest
        Nothing -> sieve ns (M.insert (n^2) [n] m)
      where
        mark m p = M.insertWith (++) (n+p) [p] m

checkOddComposites :: IO ()
checkOddComposites = check "oddComposites"
    [ take 6 oddComposites == [9,15,21,25,27,33]
    , 111 `elem` oddComposites
    ]

doubleSquares :: Integral a => [a]
doubleSquares = [2*n^2 | n <- [1..]]

checkDoubleSquares :: IO ()
checkDoubleSquares = check "doubleSquares"
    [ take 6 doubleSquares == [2,8,18,32,50,72]
    , doubleSquares !! 999 == 2000000
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
    let ps = [2,3,5,7]
    in n `elem` ps ||
       all (\p -> n `mod` p /= 0) ps &&
       millerRabin (toInteger n) (map toInteger ps)

checkSmallPrime :: IO ()
checkSmallPrime = check "smallPrime"
    [ all smallPrime [2,3,5,7,11,13]
    , not (any smallPrime [-1,0,1,4,9])
    ]

sumOfPrimeDoubleSquare :: Int -> Bool
sumOfPrimeDoubleSquare n =
    let ds = takeWhile (< n) doubleSquares
    in s' n ds
  where
    s' _ []     = False
    s' n (d:ds) =
       if smallPrime (n-d)
       then True
       else s' n ds

checkSumOfPrimeDoubleSquare :: IO ()
checkSumOfPrimeDoubleSquare = check "sumOfPrimeDoubleSquare"
    [ all sumOfPrimeDoubleSquare [9,15,21,25,27,33]
    , not (any sumOfPrimeDoubleSquare [1,2,3,6,8,12])
    ]

test :: IO ()
test = do
    checkOddComposites
    checkDoubleSquares
    checkModExp
    checkSmallPrime
    checkSumOfPrimeDoubleSquare

main :: IO ()
main = case (find (not . sumOfPrimeDoubleSquare) oddComposites) of
    Just n -> print n
