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
import GHC.OldList (all)

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
matchString (s:ss) (t:ts) = if s == t then matchString ss ts else Nothing

checkAll :: String -> [Bool]
checkAll "" = []
checkAll s@(_:t) = isJust (matchString "XMAS" s <|> matchString (reverse "XMAS") s) : checkAll t

offset :: [String] -> [String]
offset [] = []
offset (s:ss) = s : (('#':) <$> offset ss)

diagonalize :: [String] -> [String]
diagonalize = transpose . offset . (fmap reverse) . reverse . offset . (fmap reverse) . reverse

main :: IO ()
main = do
    input <- parseLines <$> getContents
    let a = input ++ transpose input ++ diagonalize input ++ diagonalize (reverse input)
    print $ length $ concatMap (filter id . checkAll) a