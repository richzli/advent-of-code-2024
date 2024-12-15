import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Bits
import Data.Char
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import System.IO
import Text.Read

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

type Grid = Map.Map Point Char

gps :: Point -> Int
gps (x, y) = 100 * x + y

dir :: Char -> Point
dir '^' = (-1, 0)
dir 'v' = (1, 0)
dir '>' = (0, 1)
dir '<' = (0, -1)

extend :: Grid -> Point -> Point -> (Bool, Point)
extend g d p = case Map.lookup p g of
    Just '.' -> (True, p)
    Just '#' -> (False, p)
    Just 'O' -> extend g d (p `addPoint` d)
    Just '@' -> extend g d (p `addPoint` d)
    _ -> (False, p)

move :: (Grid, Point) -> Char -> (Grid, Point)
move (g, p) c = case (ok, displace) of
    (False, _) -> (g, p)
    (True, False) -> (g', p `addPoint` d)
    (True, True) -> (Map.insert p' 'O' g', p `addPoint` d)
    where
        d = dir c
        (ok, p') = extend g d p
        displace = Map.lookup (p `addPoint` d) g == Just 'O'
        g' = Map.insert (p `addPoint` d) '@' $ Map.insert p '.' g

main :: IO ()
main = do
    input <- parseLines <$> getContents
    let [grid', moves'] =  split null input
    let grid = Map.fromList $ enumerate2d grid'
    let moves = concat moves'
    let p = fst $ head $ Map.toList $ Map.filterWithKey (\k v -> v == '@') grid

    print $ sum $ fmap (gps . fst) $ Map.toList $ Map.filter (=='O') . fst $ foldl' move (grid, p) moves