module Main where

check :: String -> [Bool] -> IO ()
check name tests =
    if and tests
    then return ()
    else error name

type Year = Int

data Month
    = Jan
    | Feb
    | Mar
    | Apr
    | May
    | Jun
    | Jul
    | Aug
    | Sep
    | Oct
    | Nov
    | Dec
    deriving (Enum, Eq, Ord, Show)

data Day
    = Sun
    | Mon
    | Tue
    | Wed
    | Thu
    | Fri
    | Sat
    deriving (Enum, Eq, Ord, Show)

data Date = Date
    { year  :: Year
    , month :: Month
    , day   :: Int
    } deriving (Eq, Show)

leap :: Year -> Bool
leap y = y `mod` 4 == 0 && (y `mod` 100 /= 0 || y `mod` 400 == 0)

checkLeap :: IO ()
checkLeap = check "leap"
    [ not (leap 1900)
    , leap 1904
    , leap 2000
    ]

daysPerMonth :: Year -> Month -> Int
daysPerMonth y Feb
    | leap y     = 29
daysPerMonth _ m = case m of
    Jan -> 31
    Feb -> 28
    Mar -> 31
    Apr -> 30
    May -> 31
    Jun -> 30
    Jul -> 31
    Aug -> 31
    Sep -> 30
    Oct -> 31
    Nov -> 30
    Dec -> 31

date :: Year -> Month -> Int -> Date
date y m d =
    if y < 1900 ||
       d < 1 ||
       d > daysPerMonth y m
    then error "invalid date"
    else Date y m d

months :: [Month]
months = map toEnum [0..11]

daysPerYear :: Year -> Int
daysPerYear y = sum (map (daysPerMonth y) months)

checkDaysPerYear :: IO ()
checkDaysPerYear = check "daysPerYear"
    [ daysPerYear 1900 == 365
    , daysPerYear 1904 == 366
    , daysPerYear 2000 == 366
    ]

dateToDay :: Date -> Day
dateToDay (Date y m d) = toEnum ((fromEnum Mon + n) `mod` 7)
  where
    n = sum (map daysPerYear [1900..y-1]) +
        sum (map (daysPerMonth y) (takeWhile (< m) months)) +
        d - 1

checkDateToDay :: IO ()
checkDateToDay = check "dateToDay"
    [ dateToDay (date 1900 Jan 1) == Mon
    , dateToDay (date 2000 Jan 1) == Sat
    , dateToDay (date 2018 Feb 9) == Fri
    ]

firsts :: [Date]
firsts = [date y m 1 | y <- [1901..2000], m <- months]

checkFirsts :: IO ()
checkFirsts = check "firsts"
    [ take 3 firsts ==
        [ date 1901 Jan 1
        , date 1901 Feb 1
        , date 1901 Mar 1
        ]
    , last firsts == date 2000 Dec 1
    ]

test :: IO ()
test = do
    checkLeap
    checkDaysPerYear
    checkDateToDay
    checkFirsts

main :: IO ()
main = print (length (filter (==Sun) (map dateToDay firsts)))
