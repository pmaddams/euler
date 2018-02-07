import Control.Exception
import Data.Char
import Data.Vector (Vector)
import qualified Data.Vector as Vector

type Grid = Vector (Vector Int)

check :: String -> [Bool] -> IO ()
check name tests = assert (and tests) $
    putStrLn (name ++ ": tests passed")

fromLOL :: [[Int]] -> Grid
fromLOL nss = Vector.fromList (map Vector.fromList nss)

test :: IO ()
test = check "" []

main :: IO ()
main = do
    s <- readFile "11.txt"
    ss <- return (lines s)
    sss <- return (map words ss)
    nss <- return (map (map read) sss :: [[Int]])
    g <- return (fromLOL nss)
    print g
