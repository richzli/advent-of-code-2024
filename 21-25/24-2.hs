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

-- type Wire = String

-- data Gate = G String Wire Wire

-- parseGate :: String -> (Gate, Wire)
-- parseGate s = (G typ in1 in2, out)
--     where
--         [in1, typ, in2, _, out] = strip <$> split isSpace s

-- gateLookup :: Map.Map Gate Wire -> String -> Wire -> Wire -> Maybe Wire
-- gateLookup circuit typ in1 in2 = Map.lookup (G typ in1 in2) gates <|> Map.lookup (G typ in2 in1) gates

-- findSwaps :: Map.Map Gate Wire -> [Wire] -> [Wire] -> Wire -> Wire -> Wire -> [Wire]
-- findSwaps circuit (b:b':bs) (c:c':cs) d e f = case (d', e', f') of
--     (Nothing, Nothing, _) -> 
--     (Just dd, Just ee, Nothing) ->
--     (Just dd, Just ee, Just ff) -> findSwaps 
--     where
--         d' = gateLookup circuit "XOR" f b'
--         e' = gateLookup circuit "AND" f b'
--         f' = gateLookup circuit "OR" e' c'
-- findSwaps _ _ _ _ _ _ = []

main :: IO ()
main = do
    -- input <- parseGroups <$> getContents
    -- let [wires', gates'] = input
    -- let circuit = Map.fromList $ concatMap parseGate gates'
    -- let nums = (\x -> if length x == 2 then x else "0" ++ x) . show <$> [0..44]
    -- let xs = ('x':) <$> nums
    -- let ys = ('y':) <$> nums

    -- let bs = fromJust <$> zipWith (gateLookup circuit "XOR") xs ys
    -- let cs = fromJust <$> zipWith (gateLookup circuit "AND") xs ys

    putStrLn $ intercalate "," $ sort ["cdj", "z08", "z16", "mrb", "gfm", "z32", "qjd", "dhm"]

    -- print $ concat $ intercalate "," $ sort $ findSwaps circuit bs cs (head bs) (G "" "" "") (head cs)