-- 33. Digit cancelling fractions

module Main where

import Data.Char
import Data.List

main :: IO ()
main = print $
    let ds = filter (not . (`divisible` 10)) [11..99]
        fs = do
            d <- ds
            n <- takeWhile (< d) ds
            return (F n d)
        cs = filter cancelling fs
    in denom (product cs)

data Fraction = F {numer, denom :: Integer}

instance Num Fraction where
    fromInteger n = F n 1

    (F n1 d1) + (F n2 d2) = simplify (F (n1*d2 + n2*d1) (d1*d2))

    (F n1 d1) * (F n2 d2) = simplify (F (n1*n2) (d1*d2))

    negate (F n d) = F (-n) d

    abs f@(F n d) = if n*d < 0 then negate f else f

    signum (F n d) = fromInteger (signum (n*d))

instance Eq Fraction where
    f1 == f2 =
        let (F n1 d1) = simplify f1
            (F n2 d2) = simplify f2
        in n1 == n2 && d1 == d2

simplify :: Fraction -> Fraction
simplify (F n d) =
    let g = gcd n d
        n' = n `quot` g
        d' = d `quot` g
    in if d' < 0
       then F (-n') (-d')
       else F n' d'

cancelling :: Fraction -> Bool
cancelling f = any (== f) (cancelDigits f)

cancelDigits :: Fraction -> [Fraction]
cancelDigits (F n d) =
    let nds = toDigits n
        dds = toDigits d
    in do nd <- nds
          let n' = fromDigits (delete nd nds)
              d' = fromDigits (delete nd dds)
          return (F n' d')

divisible :: Integral a => a -> a -> Bool
n `divisible` d = n `rem` d == 0

toDigits :: Integral a => a -> [Int]
toDigits = map digitToInt . show . fromIntegral

fromDigits :: Integral a => [Int] -> a
fromDigits = fromIntegral . read . map intToDigit
