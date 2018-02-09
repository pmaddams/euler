import Control.Exception

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

mergeRows :: [Int] -> [Int] -> [Int]
mergeRows top bottom = zipWith3 best top bottom (tail bottom)
  where
    best t b1 b2 = t + (max b1 b2)

checkMergeRows :: IO ()
checkMergeRows = check "mergeRows"
    [ mergeRows [1,2] [3,4,5] == [5,7]
    , mergeRows [6,7] [10,9,8] == [16,16]
    ]

maxTrianglePath :: [[Int]] -> Int
maxTrianglePath nss = maximum (foldr1 mergeRows nss)

checkMaxTrianglePath :: IO ()
checkMaxTrianglePath = check "maxTrianglePath"
    [ maxTrianglePath [[1],[2,3],[4,5,6]] == 10
    , maxTrianglePath [[1],[2,3],[6,5,4]] == 9
    ]

test :: IO ()
test = do
    checkMergeRows
    checkMaxTrianglePath

main :: IO ()
main = do
    s <- readFile "18.txt"
    ss <- return (lines s)
    sss <- return (map words ss)
    nss <- return (map (map read) sss :: [[Int]])
    print (maxTrianglePath nss)