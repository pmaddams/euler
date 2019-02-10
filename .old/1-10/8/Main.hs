module Main where

import Data.Char

adjacent :: Int -> [a] -> [[a]]
adjacent n ds
    | length ds < n = []
    | otherwise     = take n ds : adjacent n (tail ds)

maxAdjacent :: Int -> [Int] -> Maybe Int
maxAdjacent n ds =
    let dss = adjacent n ds
    in case dss of
        [] -> Nothing
        _  -> Just (maximum (map product dss))

main :: IO ()
main = do
    s <- readFile "8.txt"
    s <- return (filter isDigit s)
    ds <- return (map digitToInt s)
    case (maxAdjacent 13 ds) of
        Nothing -> putStrLn "none found"
        Just n  -> print n
