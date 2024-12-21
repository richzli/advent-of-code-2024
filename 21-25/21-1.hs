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

arrows :: Point -> Char
arrows p = case p of
    (1, 0) -> 'v'
    (-1, 0) -> '^'
    (0, 1) -> '>'
    (0, -1) -> '<'

pad1 :: Char -> Point
pad1 c = case c of
    '7' -> (0, 0)
    '8' -> (0, 1)
    '9' -> (0, 2)
    '4' -> (1, 0)
    '5' -> (1, 1)
    '6' -> (1, 2)
    '1' -> (2, 0)
    '2' -> (2, 1)
    '3' -> (2, 2)
    '0' -> (3, 1)
    'A' -> (3, 2)

pad2 :: Char -> Point
pad2 c = case c of
    '^' -> (0, 1)
    'A' -> (0, 2)
    '<' -> (1, 0)
    'v' -> (1, 1)
    '>' -> (1, 2)

go :: Int -> Bool -> String
go dist vert = take (abs dist) $ repeat $ if vert
    then (if dist > 0 then 'v' else '^')
    else (if dist > 0 then '>' else '<')

go1 :: Char -> Char -> [String]
go1 c1 c2
    | fst p1 == 3 && snd p2 == 0 = [xx ++ yy ++ "A"]
    | fst p2 == 3 && snd p1 == 0 = [yy ++ xx ++ "A"]
    | dx > 0 && dy > 0           = [xx ++ yy ++ "A"]
    | otherwise                  = [yy ++ xx ++ "A"]
    where
        p1 = pad1 c1
        p2 = pad1 c2
        (dx, dy) = p2 `subPoint` p1
        xx = go dx True
        yy = go dy False

go2 :: Char -> Char -> [String]
go2 c1 c2
    | snd p1 == 0      = [yy ++ xx ++ "A"]
    | snd p2 == 0      = [xx ++ yy ++ "A"]
    | dx > 0 && dy > 0 = [xx ++ yy ++ "A"]
    | otherwise        = [yy ++ xx ++ "A"]
    where
        p1 = pad2 c1
        p2 = pad2 c2
        (dx, dy) = p2 `subPoint` p1
        xx = go dx True
        yy = go dy False

solve :: (Char -> Char -> [String]) -> String -> [String]
solve f s = filter ((==best) . length) solns
    where
        solns = nub $ fmap concat $ traverse (\(a,b) -> f a b) $ zip ('A':s) s
        best = minimum $ length <$> solns

main :: IO ()
main = do
    input <- parseLines <$> getContents
    let ids = (read::String->Int) . take 3 <$> input

    -- let buttons = ['<', '>', '^', 'v']
    -- let seqs = (:) <$> buttons <*> fmap singleton buttons
    -- print $ zip seqs $ length . head . concatMap (solve go2) . solve go2 <$> seqs

    print $ sum $ zipWith (*) ids $ length . head . concatMap (solve go2) . concatMap (solve go2) . concatMap (solve go1) . singleton <$> input