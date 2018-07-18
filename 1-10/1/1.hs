module Main where

check :: String -> [Bool] -> IO ()
check name tests =
    if and tests
    then return ()
    else error name

anyOf :: [(a -> Bool)] -> a -> Bool
anyOf fs x = or (fs <*> pure x)

checkAnyOf :: IO ()
checkAnyOf =
    let fs = [(== 3), (== 5)]
    in check "anyOf"
        [ anyOf fs 3
        , anyOf fs 5
        , not (anyOf fs 6)
        ]

divis :: Integral a => a -> a -> Bool
n `divis` d = n `rem` d == 0

checkDivis :: IO ()
checkDivis = check "divis"
    [ 6 `divis` 3
    , 10 `divis` 5
    , not (10 `divis` 3)
    ]

test :: IO ()
test = do
    checkAnyOf
    checkDivis

main :: IO ()
main =
    let fs = [(`divis` 3), (`divis` 5)]
        ns = filter (anyOf fs) [1..999]
    in print (sum ns)
