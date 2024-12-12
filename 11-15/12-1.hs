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

dfs :: Map.Map Point Char -> (Set.Set Point, (Int, Int)) -> Point -> (Set.Set Point, (Int, Int))
dfs grid s@(vis, (perim, area)) p = if p `elem` vis then s else
    foldr (\p' s'@(vis', (perim', area')) -> 
        case (Map.lookup p' grid == Map.lookup p grid, p' `elem` vis') of
            (False, _) -> (vis', (perim' + 1, area'))
            (True, False) -> dfs grid s' p'
            _ -> s'
    ) (Set.insert p vis, (perim, area + 1)) $ addPoint p <$> d4

solve :: Map.Map Point Char -> Set.Set Point -> [Point] -> [(Int, Int)]
solve grid vis (p:ps)
    | p `elem` vis = (0, 0) : solve grid vis ps
    | otherwise    = ans : solve grid (Set.union vis vis') ps
        where (vis', ans) = dfs grid (Set.empty, (0, 0)) p
solve _ _ [] = []

main :: IO ()
main = do
    input <- parseLines <$> getContents
    let m = length input
    let n = length $ head input
    let grid = Map.fromList $ enumerate2d input

    print $ sum $ fmap (\x -> fst x * snd x) $ solve grid Set.empty $ (,) <$> [0..(m-1)] <*> [0..(n-1)]