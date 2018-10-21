module Main where

anyOf :: [(a -> Bool)] -> a -> Bool
anyOf fs x = or (fs <*> pure x)

divis :: Integral a => a -> a -> Bool
n `divis` d = n `rem` d == 0

main :: IO ()
main =
    let fs = [(`divis` 3), (`divis` 5)]
        ns = filter (anyOf fs) [1..999]
    in print (sum ns)
