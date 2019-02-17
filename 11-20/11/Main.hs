-- 11. Largest product in a grid

module Main where

import Control.Monad
import Data.Array

type Coordinate = (Int, Int)

type Direction = Coordinate -> Coordinate

type Grid a = Array Coordinate a

main :: IO ()
main = do
    grid <- readGrid "11.txt"
    let ns = map product (connect 4 grid)
    print (maximum ns)

connect :: Int -> Grid a -> [[a]]
connect n grid = do
    start <- range (bounds grid)
    dir <- [east, southeast, south, southwest]
    let xs = (walk n grid dir start)
    guard (not (null xs))
    return xs

walk :: Int -> Grid a -> Direction -> Coordinate -> [a]
walk n grid dir start = do
    let coords = take n (iterate dir start)
    guard (all (inRange (bounds grid)) coords)
    coord <- coords
    return (grid ! coord)

east :: Direction
east (i, j) = (i, j+1)

southeast :: Direction
southeast (i, j) = (i+1, j+1)

south :: Direction
south (i, j) = (i+1, j)

southwest :: Direction
southwest (i, j) = (i+1, j-1)

makeGrid :: [[a]] -> Grid a
makeGrid xss@(xs:_) =
    let m = length xss
        n = length xs
    in listArray ((1, 1), (m, n)) (concat xss)

readGrid :: (Integral a, Read a) => FilePath -> IO (Grid a)
readGrid name = readFile name >>= f
  where
    f = return . makeGrid . map (map read) . map words . lines
