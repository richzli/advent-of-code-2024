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

elemBox :: Point -> Set.Set Point -> Bool
elemBox p s = p `elem` s || (p `addPoint` (0, 1)) `elem` s

erase :: [Point] -> Grid -> Grid
erase l g = foldr (\p g' -> Map.insert p '.' g') g $ concatMap ((<*>) [id, addPoint (0, 1)] . pure) l

place :: [Point] -> Grid -> Grid
place l g = foldr (\p g' -> Map.insert p ']' g') (foldr (\p g' -> Map.insert p '[' g') g l) (addPoint (0, 1) <$> l)

merge :: (Bool, Set.Set Point) -> (Bool, Set.Set Point) -> (Bool, Set.Set Point)
merge (ok, s) (ok', s') = (ok && ok', Set.union s s')

extend :: Grid -> Set.Set Point -> Point -> Point -> (Bool, Set.Set Point)
extend g s d p = case Map.lookup p g of
    Just '.' -> (True, Set.empty)
    Just '#' -> (False, Set.empty)
    Just '@' -> extend g s d (p `addPoint` d)
    Just '[' -> case d of
        (0, _) -> Set.insert p <$> extend g s d (p `addPoint` d)
        (_, 0) ->
            let p2 = p `addPoint` (0, 1)
                x1@(ok1, s1) = extend g s d (p `addPoint` d)
                s2 = Set.union s s1
            in Set.insert p <$> if not (p2 `elemBox` s2) then merge x1 (extend g s2 d (p2 `addPoint` d)) else x1
    Just ']' -> case d of
        (0, _) -> extend g s d (p `addPoint` d)
        (_, 0) ->
            let p2 = p `addPoint` (0, -1)
                x1@(ok1, s1) = extend g s d (p `addPoint` d)
                s2 = Set.union s s1
            in Set.insert p2 <$> if not (p2 `elemBox` s2) then merge x1 (extend g s2 d (p2 `addPoint` d)) else x1
    _ -> (False, Set.empty)

move :: (Grid, Point) -> Char -> (Grid, Point)
move (g, p) c = if ok then (moveRobot $ place (addPoint d <$> boxes) $ erase boxes g, p `addPoint` d) else (g, p)
    where
        d = dir c
        (ok, s) = extend g Set.empty d p
        boxes = Set.toList s
        moveRobot = Map.insert (p `addPoint` d) '@' . Map.insert p '.'

fatten :: Char -> String
fatten '#' = "##"
fatten 'O' = "[]"
fatten '.' = ".."
fatten '@' = "@."

main :: IO ()
main = do
    input <- parseLines <$> getContents
    let [grid', moves'] = split null input
    let grid = Map.fromList $ enumerate2d $ concatMap fatten <$> grid'
    let moves = concat moves'
    let p = fst $ head $ Map.toList $ Map.filterWithKey (\k v -> v == '@') grid

    print $ sum $ fmap (gps . fst) $ Map.toList $ Map.filter (=='[') . fst $ foldl' move (grid, p) moves