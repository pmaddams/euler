import Control.Exception
import Data.Char

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

digits :: Int -> Integer -> Integer
digits n bn = (read . (take n) . show) bn

checkDigits :: IO ()
checkDigits = check "digits"
    [ 5 `digits` 123456789 == 12345
    , 10 `digits` 123456789 == 123456789
    ]

test :: IO ()
test = checkDigits

main :: IO ()
main = do
    s <- readFile "13.txt"
    ss <- return (lines s)
    ns <- return (map read ss)
    print (10 `digits` (sum ns))
