import Control.Exception
import Data.List

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

triples :: [(Int, Int, Int)]
triples =
    [(a, b, c)
    | m <- [2..]
    , n <- [1..(m-1)]
    , let a = m^2 - n^2
    , let b = 2 * m * n
    , let c = m^2 + n^2
    ]

checkTriples :: IO ()
checkTriples = check "triples"
    [ head triples == (3, 4, 5)
    , and (map (\(a, b, c) -> a^2 + b^2 == c^2) (take 10 triples))
    ]

tripleSum :: (Int, Int, Int) -> Int
tripleSum (a, b, c) = a + b + c

checkTripleSum :: IO ()
checkTripleSum = check "tripleSum"
    [ tripleSum (3, 4, 5) == 12
    , tripleSum (5, 12, 13) == 30
    ]

test :: IO ()
test = do
    checkTriples
    checkTripleSum

main :: IO ()
main = case (find ((==1000) . tripleSum) triples) of
    Nothing        -> putStrLn "none found"
    Just (a, b, c) -> print (a*b*c)
