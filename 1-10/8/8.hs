import Control.Exception
import Data.Char

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

adjacent :: Int -> [a] -> [[a]]
adjacent n ds
    | length ds < n = []
    | otherwise     = take n ds : adjacent n (tail ds)

checkAdjacent :: IO ()
checkAdjacent = check "adjacent"
    [ adjacent 2 [1..5] == [[1,2],[2,3],[3,4],[4,5]]
    , adjacent 3 [1..5] == [[1,2,3],[2,3,4],[3,4,5]]
    ]

maxAdjacent :: Int -> [Int] -> Maybe Int
maxAdjacent n ds =
    let dss = adjacent n ds
    in case dss of
        [] -> Nothing
        _  -> Just (maximum (map product dss))

checkMaxAdjacent :: IO ()
checkMaxAdjacent = check "maxAdjacent"
    [ maxAdjacent 4 [1,9,9,8,9,0] == Just 5832
    , maxAdjacent 7 [1,9,9,8,9,0] == Nothing
    ]

test :: IO ()
test = do
    checkAdjacent
    checkMaxAdjacent

main :: IO ()
main = do
    s <- readFile "8.txt"
    s <- return (filter isDigit s)
    ds <- return (map digitToInt s)
    case (maxAdjacent 13 ds) of
        Nothing -> putStrLn "none found"
        Just n  -> print n
