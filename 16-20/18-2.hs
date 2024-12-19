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

parsePoint :: String -> Point
parsePoint = (\[x, y] -> (x, y)) . fmap read . split (==',')

inf :: Int
inf = 1 `shift` 32

bfs :: Map.Map Point Int -> Map.Map Point Int -> Int -> [Point] -> Map.Map Point Int
bfs _ vis _ [] = vis
bfs grid vis dist layer = bfs grid (Map.union vis $ Map.fromList $ fmap (,dist) $ nxt) (dist + 1) nxt
    where
        nxt = filter (\p -> bounds 71 71 p && p `Map.notMember` vis && Map.findWithDefault inf p grid > dist) $ nub $ addPoint <$> layer <*> d4

binSearch :: (Int -> Bool) -> Int -> Int -> Int
binSearch f lo hi
    | lo == hi  = lo
    | otherwise = if f mid then binSearch f mid hi else binSearch f lo (mid - 1)
    where mid = (lo + hi + 1) `div` 2

main :: IO ()
main = do
    input <- parseLines <$> getContents
    let mem = parsePoint <$> input
    let grid = Map.fromList $ (,0) <$> take 1024 mem

    let grid' = Map.fromList . fmap (,0) . (flip take) mem
    let check = (\x -> isJust $ Map.lookup (70, 70) $ bfs (grid' x) (Map.singleton (0, 0) 0) 1 [(0, 0)])

    print $ mem !! (binSearch check 0 (length mem))