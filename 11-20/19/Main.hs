module Main where

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

dateToDay :: Date -> Day
dateToDay (Date y m d) = toEnum ((fromEnum Mon + n) `mod` 7)
  where
    n = sum (map daysPerYear [1900..y-1]) +
        sum (map (daysPerMonth y) (takeWhile (< m) months)) +
        d - 1

firsts :: [Date]
firsts = [date y m 1 | y <- [1901..2000], m <- months]

main :: IO ()
main = print (length (filter (==Sun) (map dateToDay firsts)))
