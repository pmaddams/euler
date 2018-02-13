import Control.Exception
import Data.List
import Data.Ord

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

remainders :: Int -> [Int]
remainders d = iterate (\r -> (10*r) `rem` d) 1

checkRemainders :: IO ()
checkRemainders = check "remainders"
    [ take 3 (remainders 6) == [1,4,4]
    , take 7 (remainders 7) == [1,3,2,6,4,5,1]
    ]

cycleLength :: Int -> Int
cycleLength d = c' (remainders d) []
  where
    c' [] _        = 0
    c' (0:rs) _    = 0
    c' (r:rs) seen = case (elemIndex r seen) of
        Nothing -> c' rs (r:seen)
        Just i  -> (i+1)

checkCycleLength :: IO ()
checkCycleLength = check "cycleLength"
    [ cycleLength 6 == 1
    , cycleLength 7 == 6
    ]

test :: IO ()
test = do
    checkRemainders
    checkCycleLength

main :: IO ()
main =
    let pairs = [(n, cycleLength n) | n <- [1..1000]]
    in print (fst (maximumBy (comparing snd) pairs))
