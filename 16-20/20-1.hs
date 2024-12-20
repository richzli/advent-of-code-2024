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

jump2 :: [Point]
jump2 = nub $ addPoint <$> d4 <*> d4

bfs :: Map.Map Point Char -> Map.Map Point Int -> Int -> [Point] -> Map.Map Point Int
bfs _ vis _ [] = vis
bfs grid vis dist layer = bfs grid (Map.union vis $ Map.fromList $ fmap (,dist) $ nxt) (dist + 1) nxt
    where
        nxt = filter (\p -> p `Map.member` grid && p `Map.notMember` vis && Map.lookup p grid /= Just '#') $ nub $ addPoint <$> layer <*> d4

cheats :: Map.Map Point Int -> Point -> Int
cheats dists p = length $ filter ((>=(100+2)) . (d-)) $ mapMaybe (($ dists) . Map.lookup . addPoint p) jump2
    where d = fromJust $ Map.lookup p dists

main :: IO ()
main = do
    input <- parseLines <$> getContents
    let grid = Map.fromList $ enumerate2d input
    let end = fst $ head $ Map.toList $ Map.filter (=='E') $ grid
    let start = fst $ head $ Map.toList $ Map.filter (=='S') $ grid
    let spaces = fmap fst $ Map.toList $ Map.filter (/='#') $ grid
    let dists = bfs grid (Map.singleton end 0) 1 [end]

    print $ sum $ cheats dists <$> spaces
    
    