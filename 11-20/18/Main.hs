-- 18. Maximum path sum I

module Main where

main :: IO ()
main = do
    nss <- readRows "18.txt"
    let ns = foldr1 mergeRows nss
    print (maximum ns)

mergeRows :: (Num a, Ord a) => [a] -> [a] -> [a]
mergeRows [] [_]           = []
mergeRows (n:ns) (m:m':ms) = n + max m m' : mergeRows ns (m':ms)
mergeRows _ _              = error "mergeRows: invalid dimensions"

readRows :: (Integral a, Read a) => FilePath -> IO [[a]]
readRows name = readFile name >>= f
  where
    f = return . map (map read . words) . lines
