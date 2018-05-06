module Main where

import Control.Exception
import Data.List
import Data.Ord

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

primitiveTriples :: [(Int, Int, Int)]
primitiveTriples =
    [(a, b, c)
    | m <- [2..]
    , n <- [1..(m-1)]
    , odd (m+n)
    , gcd m n == 1
    , let a = m^2 - n^2
    , let b = 2 * m * n
    , let c = m^2 + n^2
    ]

checkPrimitiveTriples :: IO ()
checkPrimitiveTriples = check "primitiveTriples"
    [ head primitiveTriples == (3, 4, 5)
    , and (map (\(a,b,c) -> a^2+b^2 == c^2) (take 10 primitiveTriples))
    ]

tripleSum :: (Int, Int, Int) -> Int
tripleSum (a, b, c) = a + b + c

checkTripleSum :: IO ()
checkTripleSum = check "tripleSum"
    [ tripleSum (3, 4, 5) == 12
    , tripleSum (5, 12, 13) == 30
    ]

multiplyTripleUpTo :: Int -> (Int, Int, Int) -> [(Int, Int, Int)]
multiplyTripleUpTo n t@(a, b, c) = m' 1
  where
    m' i =
        let t' = (a*i, b*i, c*i)
        in if tripleSum t' > n
           then []
           else t' : m' (i+1)

triplesUpTo :: Int -> [(Int, Int, Int)]
triplesUpTo n = sortBy (comparing tripleSum) $
    let ts = takeWhile ((<= n) . tripleSum) primitiveTriples
    in concatMap (multiplyTripleUpTo n) ts

checkTriplesUpTo :: IO ()
checkTriplesUpTo = check "triplesUpTo"
    [ triplesUpTo 24 == [(3,4,5),(6,8,10)]
    , (30,40,50) `elem` (triplesUpTo 120)
    ]

test :: IO ()
test = do
    checkPrimitiveTriples
    checkTripleSum
    checkTriplesUpTo

main :: IO ()
main =
    let t@(a, b, c) = last (triplesUpTo 1000)
    in if tripleSum t == 1000
       then print (a*b*c)
       else putStrLn "none found"
