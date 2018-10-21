module Test where

import Main hiding (main)

test :: String -> [Bool] -> IO ()
test name cases =
    if and cases
    then return ()
    else error name

testLeap :: IO ()
testLeap = test "leap"
    [ not (leap 1900)
    , leap 1904
    , leap 2000
    ]

testDaysPerYear :: IO ()
testDaysPerYear = test "daysPerYear"
    [ daysPerYear 1900 == 365
    , daysPerYear 1904 == 366
    , daysPerYear 2000 == 366
    ]

testDateToDay :: IO ()
testDateToDay = test "dateToDay"
    [ dateToDay (date 1900 Jan 1) == Mon
    , dateToDay (date 2000 Jan 1) == Sat
    , dateToDay (date 2018 Feb 9) == Fri
    ]

testFirsts :: IO ()
testFirsts = test "firsts"
    [ take 3 firsts ==
        [ date 1901 Jan 1
        , date 1901 Feb 1
        , date 1901 Mar 1
        ]
    , last firsts == date 2000 Dec 1
    ]

main :: IO ()
main = do
    testLeap
    testDaysPerYear
    testDateToDay
    testFirsts
