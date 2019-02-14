-- 28. Number spiral diagonals

module Main where

main :: IO ()
main = print (sum ns)
  where
    ns = takeWhile (<= 1001^2) spiral

spiral :: Num a => [a]
spiral = [1] ++ loop 1 2
  where
    loop prev size =
        let corners = take 4 (tail (iterate (+ size) prev))
        in corners ++ loop (last corners) (size + 2)
