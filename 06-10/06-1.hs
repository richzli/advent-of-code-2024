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

nextPos :: (Point, Int) -> (Point, Int)
nextPos (p, d) = (p `addPoint` (d4 !! d), d)

turnRight :: (Point, Int) -> (Point, Int)
turnRight (p, d) = (p, (d + 3) `mod` 4)

simulate :: (Int, Int, Set.Set Point) -> Set.Set (Point, Int) -> (Point, Int) -> Int
simulate c@(m, n, b) s x@(p, d) = case (x `elem` s || not (bounds m n p), fst (nextPos x) `elem` b) of
    (True, _) -> Set.size $ Set.map fst s
    (False, False) -> simulate c (Set.insert x s) (nextPos x)
    (False, True) -> simulate c (Set.insert x s) (turnRight x)

main :: IO ()
main = do
    input <- parseLines <$> getContents
    let m = length input
    let n = length $ head input
    let blocks = Set.fromList $ fmap fst $ filter ((=='#') . snd) $ enumerate2d input
    let start = fst $ head $ filter ((=='^') . snd) $ enumerate2d input

    print $ simulate (m, n, blocks) Set.empty (start, 2)