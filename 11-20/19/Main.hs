-- 19. Counting Sundays

module Main where

main :: IO ()
main = print (length (filter (== Sun) ds))
  where
    ds = [dayOfWeek y m 1 | y <- [1901..2000], m <- [Jan .. Dec]]

type Year = Int

data Month
    = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
  deriving (Enum, Eq, Ord)

data Day
    = Sun | Mon | Tue | Wed | Thu | Fri | Sat
  deriving (Enum, Eq)

dayOfWeek :: Year -> Month -> Int -> Day
dayOfWeek y m d =
    if y < 1900 || d < 1 || d > daysPerMonth y m
    then error "dayOfWeek: invalid date"
    else toEnum ((fromEnum Mon + n) `rem` 7)
  where
    n = sum (map daysPerYear [1900..y-1])
        + sum (map (daysPerMonth y) (takeWhile (< m) [Jan ..]))
        + d - 1

daysPerYear :: Year -> Int
daysPerYear y = sum (map (daysPerMonth y) [Jan .. Dec])

daysPerMonth :: Year -> Month -> Int
daysPerMonth y m =
    case m of
        Jan -> 31
        Feb -> if leap y then 29 else 28
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

leap :: Year -> Bool
leap y = y `divisible` 4 &&
    (y `divisible` 400 || not (y `divisible` 100))

divisible :: Integral a => a -> a -> Bool
n `divisible` d = n `rem` d == 0
