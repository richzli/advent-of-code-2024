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

parseBlock :: [String] -> [[Int]]
parseBlock = reverse . fmap (mapMaybe (readMaybe::String->Maybe Int) . fmap strip . split (`elem` ['+', '=', ',']))

matMul :: [[Int]] -> [[Int]] -> [[Int]]
matMul a b = transpose $ (<*>) ((sum .) . zipWith (*) <$> a) . pure <$> transpose b

solve :: [[Int]] -> [Int]
solve (b:a)
    | d == 0                   = [0, 0] -- :clown:
    | all ((== 0) . (`mod` d)) c = (`div` d) <$> c
    | otherwise                = [0, 0]
    where
        [[x, y], [z, w]] = transpose a
        d = x * w - y * z
        g = gcd x z
        c = head $ transpose $ matMul [[w, -y], [-z, x]] $ transpose [b]

main :: IO ()
main = do
    input <- fmap parseBlock . split null . parseLines <$> getContents
    
    print $ sum $ sum . zipWith (*) [1, 3] . solve <$> input