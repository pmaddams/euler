import Control.Exception
import Data.Char

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

digits :: Int -> Integer -> Integer
digits n bn = (read . (take n) . show) bn :: Integer

checkDigits :: IO ()
checkDigits = check "digits"
    [ digits 5 123456789 == 12345
    , digits 10 123456789 == 123456789
    ]

test :: IO ()
test = checkDigits

main :: IO ()
main = do
    s <- readFile "13.txt"
    ss <- return (lines s)
    ns <- return (map read ss :: [Integer])
    print (10 `digits` (sum ns))
