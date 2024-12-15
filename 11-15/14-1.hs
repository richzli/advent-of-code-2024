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

halve :: [a] -> ([a], [a])
halve l = splitAt ((length l + 1) `div` 2) l

room :: [Int]
room = [101, 103]

parseRobot :: String -> ([Int], [Int])
parseRobot s = halve $ mapMaybe (readMaybe::String->Maybe Int) $ split (`elem` [',', '=', ' ']) s

move :: Int -> ([Int], [Int]) -> [Int]
move n (p, v) = zipWith (\y x -> ((x `mod` y) + y) `mod` y) room $ zipWith (+) p $ (*n) <$> v

quadrant :: [Int] -> Maybe Int
quadrant [x, y] = (+) <$> x' <*> y'
    where
        [rx, ry] = (`div` 2) <$> room
        x' = if x == rx then Nothing
                else if x < rx then Just 0 else Just 1
        y' = if y == ry then Nothing
                else if y < ry then Just 0 else Just 2

main :: IO ()
main = do
    input <- parseLines <$> getContents
    let robots = parseRobot <$> input

    print $ foldl' (*) 1 $ (<*>) ((length .) . filter . (==) <$> [0..3]) $ pure $ mapMaybe (quadrant . move 100) robots