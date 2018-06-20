module Main where

import Data.Char

check :: String -> [Bool] -> IO ()
check name tests =
    if and tests
    then return ()
    else error name

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

distinct :: Eq a => [a] -> Bool
distinct []     = True
distinct (x:xs) = not (x `elem` xs) && distinct xs

checkDistinct :: IO ()
checkDistinct = check "distinct"
    [ distinct [1,2,3]
    , not (distinct [2,2,3])
    ]

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

checkDivisibleGroups :: IO ()
checkDivisibleGroups = check "divisibleGroups"
    [ divisibleGroups 30 ==
          [0,3,0] : [0,6,0] : [0,9,0] : map toDigits [120,150..990]
    , divisibleGroups 300 == map toDigits [300,600,900]
    ]

mergeGroups :: [[Int]] -> [[Int]] -> [[Int]]
mergeGroups begins ends = concat (m' <$> begins <*> pure ends)
  where
    m' _ []            = []
    m' (b:bs) (es:ess) =
        let es' = take 2 es
        in if bs == es'
           then (b:es) : m' (b:bs) ess
           else m' (b:bs) ess

checkMergeGroups :: IO ()
checkMergeGroups = check "mergeGroups"
    [ mergeGroups [[1,2,3],[4,5,6]] [[2,3,5,7],[5,6,8,10]] ==
          [[1,2,3,5,7],[4,5,6,8,10]]
    , mergeGroups [[1,2,3],[4,5,6]] [[2,3,5,7],[2,3,4,5,6,7]] ==
          [[1,2,3,5,7],[1..7]]
    ]

test :: IO ()
test = do
    checkToDigits
    checkFromDigits
    checkDistinct
    checkDivisibleGroups
    checkMergeGroups

main :: IO ()
main =
    let gss = map divisibleGroups [1,2,3,5,7,11,13,17]
        dss = filter distinct (foldr1 mergeGroups gss)
    in print (sum (map fromDigits dss))
