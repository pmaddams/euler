module Main where

import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Maybe

primes :: Integral a => [a]
primes = sieve [2..] M.empty
  where
    sieve (n:ns) m = case M.lookup n m of
        Just ps -> sieve ns (foldl mark (M.delete n m) ps)
        Nothing -> n : sieve ns (M.insert (n^2) [n] m)
      where
        mark m p = M.insertWith (++) (n+p) [p] m

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

millerRabin :: Integer -> [Integer] -> Bool
millerRabin n as = all (\a -> modExp a (n-1) n == 1) as

prime :: Integral a => a -> Bool
prime n = n > 1 &&
    let ps = [2,3,5,7]
    in n `elem` ps ||
       all (\p -> n `mod` p /= 0) ps &&
       millerRabin (toInteger n) (map toInteger ps)

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral

fromDigits :: Integral a => [Int] -> a
fromDigits = fromIntegral . read . concatMap show

uniqueDigits :: [Int] -> [Int]
uniqueDigits ns = nub $ sort $
    map (fromDigits . sort . toDigits) ns

primePermutations :: Int -> [Int]
primePermutations n =
    let ds = permutations (toDigits n)
        ns = nub (sort (map fromDigits ds))
    in filter prime ns

digitPermutationOf :: Int -> Int -> Bool
n `digitPermutationOf` m =
    let d' = sort . toDigits
    in d' n == d' m

primePermutationSequence :: Int -> Maybe (Int, Int, Int)
primePermutationSequence = p' . primePermutations
  where
    p' []     = Nothing
    p' (n:ns) =
        let ps = p'' n ns
        in if (isNothing ps)
           then p' ns
           else ps

    p'' n []       = Nothing
    p'' n (n':ns') =
        let n'' = 2*n' - n
        in if prime n'' &&
              n'' `digitPermutationOf` n
           then Just (n, n', n'')
           else p'' n ns'

findPrimePermutationSequence :: [Int] -> Maybe (Int, Int, Int)
findPrimePermutationSequence []     = Nothing
findPrimePermutationSequence (n:ns) =
    let ps = primePermutationSequence n
    in if (isNothing ps)
       then findPrimePermutationSequence ns
       else ps

main :: IO ()
main =
    let ps = dropWhile (<= 1487) (uniqueDigits (takeWhile (< 10000) primes))
    in case (findPrimePermutationSequence ps) of
        Just (n, n', n'') -> print (fromDigits (concatMap toDigits [n, n', n'']))
