import Control.Exception
import Data.List

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

multiplyAll :: [Int] -> [Int]
multiplyAll []     = []
multiplyAll (n:ns) = (map (*n) (n:ns)) ++ multiplyAll ns

checkMultiplyAll :: IO ()
checkMultiplyAll = check "multiplyAll"
    [ multiplyAll [1..3] == [1,2,3,4,6,9]
    , multiplyAll [4..6] == [16,20,24,25,30,36]
    ]

palindrome :: Int -> Bool
palindrome n = let s = show n in s == reverse s

checkPalindrome :: IO ()
checkPalindrome = check "palindrome"
    [ palindrome 12321
    , not (palindrome 12345)
    ]

largestP :: [Int] -> Maybe Int
largestP ns =
    let products = sortBy (flip compare) (multiplyAll ns)
    in case (dropWhile (not . palindrome) products) of
        []    -> Nothing
        (n:_) -> Just n

checkLargestP :: IO ()
checkLargestP = check "largestP"
    [ largestP [10..99] == Just 9009
    , largestP [] == Nothing
    ]

test :: IO ()
test = do
    checkMultiplyAll
    checkPalindrome
    checkLargestP

main :: IO ()
main = case (largestP [100..999]) of
    Nothing -> putStrLn "none found"
    Just n  -> print n
