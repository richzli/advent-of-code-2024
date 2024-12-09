import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Char
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import System.IO

--- string parsing ---

split :: (a -> Bool) -> [a] -> [[a]]
split p s = case break p s of
    (a, []) -> [a]
    (a, b) -> a : split p (drop 1 b)

strip :: String -> String
strip = dropWhile isSpace . dropWhileEnd isSpace

parseLines :: String -> [String]
parseLines = dropWhileEnd null . split (=='\n')

parseGroups :: String -> [[String]]
parseGroups = split null . parseLines

parseNumbers :: String -> [Int]
parseNumbers s = read <$> filter (/="") (split isSpace s)

--- utility ---

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

enumerate2d :: [[a]] -> [(Point, a)]
enumerate2d grid = [((x, y), c)
    | (x, row) <- enumerate grid
    , (y, c) <- enumerate row
    ]

--- coordinates ---

type Point = (Int, Int)

d4, d8 :: [Point]
d4 = [(1, 0), (0, 1), (-1, 0), (0, -1)]
d8 = [(1, 1), (1, 0), (1, -1), (0, 1), (0, -1), (-1, 1), (-1, 0), (-1, -1)]

zeroPoint :: Point
zeroPoint = (0, 0)

addPoint :: Point -> Point -> Point
addPoint (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

subPoint :: Point -> Point -> Point
subPoint (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

mulPoint :: Int -> Point -> Point
mulPoint c (x, y) = (c*x, c*y)

neighbors :: [Point] -> Point -> [Point]
neighbors d p = addPoint p <$> d

bounds :: Int -> Int -> Point -> Bool
bounds m n (x, y) = x >= 0 && x < m && y >= 0 && y < n

---

separate :: [a] -> ([a], [a])
separate (x:y:zs) = (x:xs, y:ys)
    where (xs, ys) = separate zs
separate [x] = ([x], [])
separate [] = ([], [])

interleave :: [a] -> [a] -> [a]
interleave l1 l2 = concat $ transpose [l1, l2]

mince :: [Int] -> [(Int, Int)] -> [[(Int, Int)]]
mince (cut:cuts) (f@(fid, len):fs)
    | len == cut = [f] : mince cuts fs
    | len < cut  = case mince ((cut - len):cuts) fs of
        [] -> [[f]]
        (y:ys) -> (f:y) : ys
    | len > cut  = [(fid, cut)] : mince cuts ((fid, len - cut):fs)
mince [] _ = []
mince _ [] = []

sumTo :: Int -> Int
sumTo n = n * (n - 1) `div` 2

checksum :: Int -> Int -> [(Int, Int)] -> Int
checksum limit i ((fid, len):fs) = if limit - i <= len
    then fid * (i * (limit - i) + sumTo (limit - i))
    else fid * (i * len + sumTo len) + checksum limit (i + len) fs
checksum _ _ [] = 0

main :: IO ()
main = do
    input <- fmap (read . singleton) <$> getContents
    let (files, frees) = separate input
    let ids = zip [0..] files

    print $ checksum (sum files) 0 $ concat $ interleave (singleton <$> ids) $ mince frees (reverse ids)