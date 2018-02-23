import Control.Exception
import Data.Char

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

fromDigits :: Integral a => [Int] -> a
fromDigits = fromIntegral . read . concatMap show

checkFromDigits :: IO ()
checkFromDigits = check "fromDigits"
    [ fromDigits [1..5] == 12345
    , fromDigits [9,8..1] == 987654321
    ]

digitsOf :: (Integral a, Integral b) => Int -> a -> b
n `digitsOf` bn = (fromDigits . (take n) . toDigits) bn

checkDigitsOf :: IO ()
checkDigitsOf = check "digitsOf"
    [ 5 `digitsOf` 123456789 == 12345
    , 10 `digitsOf` 123456789 == 123456789
    ]

test :: IO ()
test = do
    checkToDigits
    checkFromDigits
    checkDigitsOf

main :: IO ()
main = do
    s <- readFile "13.txt"
    ss <- return (lines s)
    ns <- return (map read ss)
    print (10 `digitsOf` (sum ns))
