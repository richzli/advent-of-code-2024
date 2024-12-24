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

type Wire = String

data Gate = G String Wire Wire Wire

parseWire :: String -> (Wire, Int)
parseWire s = (name, read val)
    where
        [name, val] = strip <$> split (==':') s

parseGate :: String -> [(Wire, [Gate])]
parseGate s = (, [G typ in1 in2 out]) <$> [in1, in2]
    where
        [in1, typ, in2, _, out] = strip <$> split isSpace s

gateEval :: Map.Map Wire Int -> Gate -> Maybe (Wire, Int)
gateEval values (G typ in1 in2 out) = if Map.member in1 values && Map.member in2 values
    then case typ of
        "AND" -> Just (out, (.&.) i1 i2)
        "OR" -> Just (out, (.|.) i1 i2)
        "XOR" -> Just (out, xor i1 i2)
    else Nothing
    where
        i1 = fromJust $ Map.lookup in1 values
        i2 = fromJust $ Map.lookup in2 values

simulate :: Map.Map Wire [Gate] -> [(Wire, Int)] -> Map.Map Wire Int -> Map.Map Wire Int
simulate _ [] values = values
simulate circuit ((w,v):us) values = simulate circuit ((us ++) $ mapMaybe (gateEval values') $ Map.findWithDefault [] w circuit) values'
    where
        values' = Map.insert w v values

binToInt :: [Int] -> Int
binToInt [] = 0
binToInt (b:bs) = b + 2 * binToInt bs

main :: IO ()
main = do
    input <- parseGroups <$> getContents
    let [wires', gates'] = input
    let wires = parseWire <$> wires'
    let circuit = Map.fromListWith (++) $ concatMap parseGate gates'

    print $ binToInt $ fmap snd $ Map.toAscList $ Map.filterWithKey (const . (=='z') . head) $ simulate circuit wires Map.empty