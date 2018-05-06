module Main where

import Control.Exception
import Data.List

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

powers :: [Integer] -> [Integer] -> [Integer]
powers as bs = nub . sort $
    [a^b | a <- as, b <- bs]

checkPowers :: IO ()
checkPowers = check "powers"
    [ powers [1..3] [1..3] == [1,2,3,4,8,9,27]
    , powers [2..5] [2..5] ==
          [4,8,9,16,25,27,32,64,81,125,243,256,625,1024,3125]
    ]

test :: IO ()
test = checkPowers

main :: IO ()
main =
    let ns = [2..100]
    in print (length (powers ns ns))
