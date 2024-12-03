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

parseString :: String -> String -> Maybe String
parseString "" s = Just s
parseString _ "" = Nothing
parseString (s:ss) (t:ts) = if s == t then parseString ss ts else Nothing

parseInt :: String -> Maybe String
parseInt "" = Nothing
parseInt (c:cs) = if isDigit c then Just (fromMaybe cs (parseInt cs)) else Nothing

readInt :: String -> Maybe String
readInt "" = Nothing
readInt (c:cs) = if isDigit c
    then Just (c : fromMaybe "" (readInt cs))
    else Nothing

evalMul :: String -> Maybe Int
evalMul s =
    let s1 = Just s >>= parseString "mul("
        i1 = (read::String->Int) <$> (s1 >>= readInt)
        s2 = s1 >>= parseInt >>= parseString ","
        i2 = (read::String->Int) <$> (s2 >>= readInt)
        s3 = s2 >>= parseInt >>= parseString ")"
    in
        if isJust s3 then (*) <$> i1 <*> i2 else Nothing

evalAll :: String -> [Maybe Int]
evalAll "" = []
evalAll s@(_:t) = evalMul s : evalAll t

doAll :: String -> [Maybe Bool]
doAll "" = []
doAll s@(_:t) = (if isJust (parseString "do()" s) then Just True
    else if isJust (parseString "don't()" s) then Just False
    else Nothing) : doAll t

main :: IO ()
main = do
    input <- getContents
    print $ sum $ fromMaybe 0 <$> (map snd . filter fst . zip (tail $ scanl fromMaybe True (doAll input))) (evalAll input)