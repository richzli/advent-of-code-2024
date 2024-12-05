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

updateID :: [Int] -> Int
updateID l = l !! (length l `div` 2)

okHead :: Set.Set [Int] -> [Int] -> Bool
okHead _ [] = True
okHead s (h:t) = not $ or $ fmap (`elem` s) $ (:) <$> t <*> pure [h]

ok :: Set.Set [Int] -> [Int] -> Bool
ok s l = all (okHead s) (tails l)

displace :: [a] -> [[a]]
displace l = zipWith (++) (inits l) (drop 1 (tails l))

siftUp :: Set.Set [Int] -> [Int] -> [Int]
siftUp _ [] = []
siftUp s l = head $ filter (okHead s) $ zipWith (:) l (displace l)

sortUpd :: Set.Set [Int] -> [Int] -> [Int]
sortUpd _ [] = []
sortUpd s l = h : sortUpd s t
    where h:t = siftUp s l

main :: IO ()
main = do
    input <- parseLines <$> getContents
    let reqs:upds:_ = zipWith ($) (fmap . (fmap (read::String->Int) .) . split . (==) <$> ['|', ',']) (split null input)

    print $ sum $ updateID . sortUpd (Set.fromList reqs) <$> filter (not . ok (Set.fromList reqs)) upds