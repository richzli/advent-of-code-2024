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

---

matchString :: String -> String -> Maybe String
matchString "" s = Just s
matchString _ "" = Nothing
matchString (s:ss) (t:ts) = if s `elem` [t, '.'] then matchString ss ts else Nothing

x_MAS :: [String]
x_MAS = ["M.M", ".A.", "S.S"]

hohoho :: [[String]]
hohoho = merry ++ (transpose <$> merry)
    where merry = [x_MAS] ++ [(reverse x_MAS)]

matchMatrix :: [String] -> [String] -> Bool
matchMatrix m m' = all isJust $ zipWith (matchString) m m'

checkMAS :: [String] -> Bool
checkMAS m = or $ matchMatrix <$> hohoho <*> pure m

by3s :: [a] -> [[a]]
by3s l = if length l >= 3
    then (take 3 l) : (by3s (tail l))
    else []

by3x3s :: [[a]] -> [[[a]]]
by3x3s m = concatMap (foldl1 (zipWith (++)) . (fmap (fmap singleton . by3s))) $ by3s m

main :: IO ()
main = do
    input <- parseLines <$> getContents
    print $ length $ filter checkMAS $ by3x3s input