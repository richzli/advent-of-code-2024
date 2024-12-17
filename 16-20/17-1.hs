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

data Computer = Regs Int Int Int

parseComputer :: [String] -> Computer
parseComputer l = Regs a b c
    where
        [a, b, c] = (read::String->Int) . strip . head . tail . split (==':') <$> l

parseProgram :: String -> [Int]
parseProgram = fmap read . split (==',') . strip . head . tail . split (==':')

operand :: Int -> Computer -> Int
operand o (Regs a b c) = case o of
    0 -> 0
    1 -> 1
    2 -> 2
    3 -> 3
    4 -> a
    5 -> b
    6 -> c
    _ -> -1

execute :: [Int] -> Computer -> Int -> [Int]
execute l z@(Regs a b c) p
    | p >= length l - 1 = []
    | otherwise         = case op1 of
        0 -> execute l (Regs (a `div` (1 `shift` cop)) b c) p'
        1 -> execute l (Regs a (b `xor` op2) c) p'
        2 -> execute l (Regs a (cop `mod` 8) c) p'
        3 -> if a == 0 then execute l z p' else execute l z op2
        4 -> execute l (Regs a (b `xor` c) c) p'
        5 -> (cop `mod` 8) : execute l z p'
        6 -> execute l (Regs a (a `div` (1 `shift` cop)) c) p'
        7 -> execute l (Regs a b (a `div` (1 `shift` cop))) p'
    where
        op1 = l !! p
        op2 = l !! (p + 1)
        cop = operand op2 z
        p' = p + 2

main :: IO ()
main = do
    input <- parseLines <$> getContents
    let [computer', program'] = split null input
    let computer = parseComputer computer'
    let program = parseProgram $ head program'

    putStrLn $ concat $ intersperse "," $ fmap show $ execute program computer 0