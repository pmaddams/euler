module Main where

import Data.Char

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral

fromDigits :: Integral a => [Int] -> a
fromDigits = fromIntegral . read . concatMap show

distinct :: Eq a => [a] -> Bool
distinct []     = True
distinct (x:xs) = not (x `elem` xs) && distinct xs

divisibleGroups :: Int -> [[Int]]
divisibleGroups n =
    let ns = takeWhile (< 1000) (map (*n) [1..])
    in map d' (map toDigits ns)
  where
    d' ds =
        let len = length ds
        in case len of
            1 -> 0 : 0 : ds
            2 -> 0 : ds
            3 -> ds

mergeGroups :: [[Int]] -> [[Int]] -> [[Int]]
mergeGroups begins ends = concat (m' <$> begins <*> pure ends)
  where
    m' _ []            = []
    m' (b:bs) (es:ess) =
        let es' = take 2 es
        in if bs == es'
           then (b:es) : m' (b:bs) ess
           else m' (b:bs) ess

main :: IO ()
main =
    let gss = map divisibleGroups [1,2,3,5,7,11,13,17]
        dss = filter distinct (foldr1 mergeGroups gss)
    in print (sum (map fromDigits dss))
