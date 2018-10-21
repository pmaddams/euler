module Main where

import Control.Monad
import Data.List
import Data.Ord

primitiveTriples :: [[Int]]
primitiveTriples = do
    n <- [2..]
    m <- [1..(n-1)]

    guard (odd (n+m))
    guard (gcd n m == 1)

    let a = n^2 - m^2
        b = 2*n*m
        c = n^2 + m^2

    return [a, b, c]

multiplyTripleUpTo :: Int -> [Int] -> [[Int]]
multiplyTripleUpTo n t =
    let ms = [map (*i) t | i <- [1..]]
    in takeWhile ((<= n) . sum) ms

triplesUpTo :: Int -> [[Int]]
triplesUpTo n = sortBy (comparing sum) $
    let ts = takeWhile ((<= n) . sum) primitiveTriples
    in concatMap (multiplyTripleUpTo n) ts

main :: IO ()
main =
    let t = last (triplesUpTo 1000)
    in if sum t == 1000
       then print (product t)
       else putStrLn "none found"
