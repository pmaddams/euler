import Control.Exception
import Data.Char
import Data.List
import Data.Maybe

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

maybeMultiplePD :: Int -> Maybe Int
maybeMultiplePD n =
    if n < 1
    then Nothing
    else m' 1 []
  where
    m' i acc =
        let acc' = acc ++ toDigits (n*i)
            len = length acc'
        in if len < 9
           then m' (i+1) acc'
           else if len == 9 && sort acc' == [1..9]
           then Just (fromDigits acc')
           else Nothing

checkMaybeMultiplePD :: IO ()
checkMaybeMultiplePD = check "maybeMultiplePD"
    [ maybeMultiplePD 192 == Just 192384576
    , maybeMultiplePD 9 == Just 918273645
    ]

test :: IO ()
test = do
    checkToDigits
    checkFromDigits
    checkMaybeMultiplePD

main :: IO ()
main =
    let multiplesPD = catMaybes (map maybeMultiplePD [1..9999])
    in print (maximum multiplesPD)