import Control.Exception
import Data.Char
import Data.List

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

toList :: Int -> [Int]
toList = map digitToInt . show

checkToList :: IO ()
checkToList = check "toList"
    [ toList 12345 == [1..5]
    , toList 987654321 == [9,8..1]
    ]

fromList :: [Int] -> Int
fromList = read . concatMap show
 
checkFromList :: IO ()
checkFromList = check "fromList"
    [ fromList [1..5] == 12345
    , fromList [9,8..1] == 987654321
    ]

pandigitals :: [(Int, Int, Int)]
pandigitals =
    [ (m1, m2, p)
    | ns <- permutations [1..9]
    , let (lo, hi) = splitAt 5 ns
    , i <- [1, 2]
    , let (ll, lh) = splitAt i lo
          m1 = fromList ll
          m2 = fromList lh
          p = m1 * m2
    , sort (toList p) == hi
    ]

test :: IO ()
test = do
    checkToList
    checkFromList

main :: IO ()
main = print (sum (nub [p | (_,_,p) <- pandigitals]))
