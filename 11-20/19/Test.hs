module Test where

import Main hiding (main)

main :: IO ()
main = do
    testDayOfWeek
    testDaysPerYear
    testLeap
    testDivisible

testDayOfWeek :: IO ()
testDayOfWeek = test "dayOfWeek"
    [ dayOfWeek 1900 Jan 1 == Mon
    , dayOfWeek 2000 Jan 1 == Sat
    , dayOfWeek 2019 Feb 13 == Wed
    ]

testDaysPerYear :: IO ()
testDaysPerYear = test "daysPerYear"
    [ daysPerYear 1900 == 365
    , daysPerYear 1904 == 366
    , daysPerYear 2000 == 366
    ]

testLeap :: IO ()
testLeap = test "leap"
    [ not (leap 1900)
    , leap 1904
    , leap 2000
    ]

testDivisible :: IO ()
testDivisible = test "divisible"
    [ 4 `divisible` 2
    , not (7 `divisible` 3)
    , 10 `divisible` 5
    , not (13 `divisible` 7)
    ]

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name
