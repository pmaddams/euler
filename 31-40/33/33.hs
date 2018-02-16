import Control.Exception
import Data.Char

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

toDigits :: Int -> [Int]
toDigits = map digitToInt . show

checkToDigits :: IO ()
checkToDigits = check "toDigits"
    [ toDigits 12345 == [1..5]
    , toDigits 987654321 == [9,8..1]
    ]

lowestTerms :: (Int, Int) -> (Int, Int)
lowestTerms (n, d) =
    let g = gcd n d
    in (n `div` g, d `div` g)

checkLowestTerms :: IO ()
checkLowestTerms = check "lowestTerms"
    [ lowestTerms (49, 98) == (1, 2)
    , lowestTerms (4, 8) == (1, 2)
    ]

equivalent :: (Int, Int) -> (Int, Int) -> Bool
equivalent r1 r2 = lowestTerms r1 == lowestTerms r2

checkEquivalent :: IO ()
checkEquivalent = check "equivalent"
    [ equivalent (49, 98) (4, 8)
    , not (equivalent (94, 89) (4, 8))
    ]

cancelling :: (Int, Int) -> Bool
cancelling (n, d)
    | n2 == 0   = False
    | n1 == d1  = e' (n2, d2)
    | n1 == d2  = e' (n2, d1)
    | n2 == d1  = e' (n1, d2)
    | n2 == d2  = e' (n1, d1)
    | otherwise = False
  where
    (n1:n2:_) = toDigits n

    (d1:d2:_) = toDigits d

    e' = equivalent (n, d)

checkCancelling :: IO ()
checkCancelling = check "cancelling"
    [ cancelling (49, 98)
    , not (cancelling (30, 50))
    ]

cancellingFractions :: [(Int, Int)]
cancellingFractions =
    let fractions = [(n, d) | d <- [12..99], n <- [11..d-1]]
    in filter cancelling fractions

multiplyFractions :: (Int, Int) -> (Int, Int) -> (Int, Int)
multiplyFractions (n, d) (n', d') = lowestTerms (n*n', d*d')

checkMultiplyFractions :: IO ()
checkMultiplyFractions = check "multiplyFractions"
    [ multiplyFractions (1, 2) (5, 10) == (1, 4)
    , multiplyFractions (3, 4) (2, 3) == (1, 2)
    ]

test :: IO ()
test = do
    checkToDigits
    checkLowestTerms
    checkEquivalent
    checkCancelling
    checkMultiplyFractions

main :: IO ()
main = print (snd (foldr1 multiplyFractions cancellingFractions))
