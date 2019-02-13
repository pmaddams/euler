-- 24. Lexicographic permutations

module Main where

import Data.List

main :: IO ()
main = putStrLn (ss !! 999999)
  where
    ss = lexPermutations "0123456789"

lexPermutations :: Ord a => [a] -> [[a]]
lexPermutations = sort . permutations
