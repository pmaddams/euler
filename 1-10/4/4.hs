import Control.Exception
import Data.Char
import Data.List

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral

checkToDigits :: IO ()
checkToDigits = check "toDigits"
    [ toDigits 12345 == [1..5]
    , toDigits 987654321 == [9,8..1]
    ]

palindrome :: Eq a => [a] -> Bool
palindrome xs = xs == reverse xs

checkPalindrome :: IO ()
checkPalindrome = check "palindrome"
    [ palindrome (toDigits 12321)
    , not (palindrome (toDigits 12345))
    ]

multiplyAll :: Num a => [a] -> [a]
multiplyAll []     = []
multiplyAll (n:ns) = (map (*n) (n:ns)) ++ multiplyAll ns

checkMultiplyAll :: IO ()
checkMultiplyAll = check "multiplyAll"
    [ multiplyAll [1..3] == [1,2,3,4,6,9]
    , multiplyAll [4..6] == [16,20,24,25,30,36]
    ]

largestP :: Integral a => [a] -> Maybe a
largestP ns = find (palindrome . toDigits) $
    sortBy (flip compare) (multiplyAll ns)

checkLargestP :: IO ()
checkLargestP = check "largestP"
    [ largestP [10..99] == Just 9009
    , largestP [] == Nothing
    ]

test :: IO ()
test = do
    checkToDigits
    checkPalindrome
    checkMultiplyAll
    checkLargestP

main :: IO ()
main = case (largestP [100..999]) of
    Nothing -> putStrLn "none found"
    Just n  -> print n
