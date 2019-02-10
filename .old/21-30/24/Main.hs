module Main where

import Data.List

lexPerms :: [Int] -> [String]
lexPerms = sort . permutations . concatMap show

main :: IO ()
main = print ((lexPerms [0..9]) !! 999999)
