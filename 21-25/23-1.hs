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

parseLink :: String -> (String, String)
parseLink s = (take 2 s, drop 3 s)

insertLink :: (String, String) -> Map.Map String (Set.Set String) -> Map.Map String (Set.Set String)
insertLink (u, v) = Map.insertWith Set.union u (Set.singleton v) . Map.insertWith Set.union v (Set.singleton u)

pairs :: Set.Set String -> [(String, String)]
pairs s = filter (\x -> fst x /= snd x) $ (,) <$> s' <*> s'
    where
        s' = Set.toList s

inGraph :: Map.Map String (Set.Set String) -> (String, String) -> Bool
inGraph m (u, v) = fromMaybe False $ fmap (Set.member v) $ Map.lookup u m

main :: IO ()
main = do
    input <- fmap parseLink . parseLines <$> getContents
    let computers = nub $ (fst <$> input) ++ (snd <$> input)
    let g = foldr insertLink Map.empty $ input

    let historian = filter ((=='t') . head) computers
    print $ length $ nub $ concat $ zipWith (fmap . Set.insert) historian $ fmap (fmap (\(u, v) -> Set.fromList [u, v]) . filter (inGraph g) . pairs) $ ($ g) . Map.findWithDefault Set.empty <$> historian