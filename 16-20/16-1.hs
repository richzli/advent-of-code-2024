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

type PQ a = Map.Map Int [a]

pqEmpty :: PQ a -> Bool
pqEmpty = Map.null

pqTop :: PQ a -> (Int, a)
pqTop = fmap head . Map.findMin

pqPop :: PQ a -> PQ a
pqPop pq
    | null (tail v) = Map.delete k pq
    | otherwise     = Map.adjust tail k pq
    where
        (k, v) = Map.findMin pq

pqInsert :: (a, Int) -> PQ a -> PQ a
pqInsert (v, p) = Map.insertWith (++) p [v]

type Pos = (Point, Int)

turn :: Int -> Pos -> Maybe Pos
turn = (Just .) . fmap . (((`mod` 4) .) . (+))

advance :: Map.Map Point Char -> Pos -> Maybe Pos
advance grid (p, d) = case Map.lookup p' grid of
    Nothing -> Nothing
    Just '#' -> Nothing
    _ -> Just (p', d)
    where
        p' = p `addPoint` (d4 !! d)

dijk :: Map.Map Point Char -> Map.Map Pos Int -> PQ Pos -> Map.Map Pos Int
dijk grid vis pq
    | pqEmpty pq = vis
    | otherwise = if Map.member p vis
        then dijk grid vis pq'
        else dijk grid (Map.insert p d vis) (foldr pqInsert pq' $ catMaybes $ [fmap (,d+1000) . turn 1, fmap (,d+1000) . turn 3, fmap (,d+1) . advance grid] <*> pure p)
    where
        (d, p) = pqTop pq
        pq' = pqPop pq

main :: IO ()
main = do
    input <- parseLines <$> getContents
    let grid = Map.fromList $ enumerate2d input
    let p = fst $ head $ Map.toList $ Map.filter (=='S') grid
    let q = fst $ head $ Map.toList $ Map.filter (=='E') grid

    print $ minimum $ fmap snd $ filter ((==q) . fst . fst) $ Map.toList $ dijk grid Map.empty (Map.fromList [(0, [(p, 1)])])