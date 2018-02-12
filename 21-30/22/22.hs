import Control.Exception
import Data.Char
import Data.List
import Data.List.Split

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

letterScore :: Char -> Int
letterScore c
    | not (isAlpha c) = 0
    | isUpper c       = ord c - ord 'A' + 1
    | isLower c       = ord c - ord 'a' + 1

checkLetterScore :: IO ()
checkLetterScore = check "letterScore"
    [ letterScore 'a' == 1
    , letterScore 'Z' == 26
    ]

wordScore :: String -> Int
wordScore = sum . (map letterScore)

checkWordScore :: IO ()
checkWordScore = check "wordScore"
    [ wordScore "COLIN" == 53
    , wordScore "Pavan" == 54
    ]

multiplyPosition :: [Int] -> [Int]
multiplyPosition = zipWith (*) [1..]

checkMultiplyPosition :: IO ()
checkMultiplyPosition = check "multiplyPosition"
    [ multiplyPosition [4,5,6] == [4,10,18]
    , multiplyPosition [7,8,9,10] == [7,16,27,40]
    ]

test :: IO ()
test = do
    checkLetterScore
    checkMultiplyPosition

main :: IO ()
main = do
    s <- readFile "22.txt"
    ss <- return (sort (splitOn "," s))
    print (sum (multiplyPosition (map wordScore ss)))