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

parseProgram :: String -> [Int]
parseProgram = fmap read . split (==',') . strip . head . tail . split (==':')

type Possibility = Map.Map Int Int

bitsToInt3 :: [Int] -> Int
bitsToInt3 = sum . zipWith (*) ((1 `shift`) <$> [0..2])

intToBits3 :: Int -> [Int]
intToBits3 = (<*>) (((`mod` 2) .) . flip shift . (0-) <$> [0..2]) . pure

cases :: Int -> Possibility -> [Int]
cases offset p = fmap bitsToInt3 $ traverse (\x -> if x == Nothing then [0, 1] else [fromJust x]) $ Map.lookup <$> ((offset+) <$> [0..2]) <*> pure p

check :: (Int, Int) -> Possibility -> Maybe Possibility
check (index, val) p = case Map.lookup index p of
    Nothing -> Just (Map.insert index val p)
    Just x -> if x == val then Just p else Nothing

fillPossibility :: Possibility -> (Int, Int) -> Int -> Maybe Possibility
fillPossibility p (nextOut, offset) proposal = foldr (flip (>>=) . check) p' $ zip (((offset + b)+) <$> [0..]) (intToBits3 (b' `xor` nextOut))
    where
        p' = Just $ Map.union (Map.fromList $ zip ((offset+) <$> [0..]) (intToBits3 proposal)) p
        b = proposal `xor` 1
        b' = proposal `xor` 5

propagate :: Possibility -> (Int, Int) -> [Possibility]
propagate p s@(nextOut, offset) = mapMaybe (fillPossibility p s) $ cases offset p

collapse :: Possibility -> Int
collapse = Map.foldrWithKey (\index val n -> n + (val `shift` index)) 0

main :: IO ()
main = do
    input <- parseLines <$> getContents
    let [computer', program'] = split null input
    let program = zip (parseProgram $ head program') $ (3*) <$> [0..]

    print $ minimum $ fmap collapse $ foldl' (\l x -> concatMap ((flip propagate) x) l) [Map.empty] program