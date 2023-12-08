{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Day3 where

import Data.Char
import Data.Either (partitionEithers)
import Data.Maybe (isJust, maybeToList)
import System.Environment (getArgs)

-- representation of values
data EnginePart = EnginePart
    { part :: String
    , ln :: Int
    , scol :: Int
    , ecol :: Int
    }
    deriving (Show)

data Schematic = Schematic
    { eps :: [EnginePart]
    , sym :: [((Int, Int), Char)]
    }
    deriving (Show)

-- mk a tuple of number/symbol w indexes
helper1 :: String -> [Either ((Int, Int), Char) EnginePart]
helper1 xs = insideH xs 0 0
  where
    insideH [] _ _ = []
    insideH (c : cs) x y
        | c == '\n' = insideH cs 0 (y + 1)
        | c == '.' = insideH cs (x + 1) y
        | isDigit c =
            let (part, left) = (c : takeWhile isDigit cs, dropWhile isDigit cs)
                ln = y
                scol = x
                ecol = x + length part - 1
             in Right EnginePart{..} : insideH left (x + length part) y
        | otherwise = Left ((y, x), c) : insideH cs (x + 1) y

near :: EnginePart -> [(Int, Int)]
near EnginePart{..} = [(l, col) | l <- [pred ln .. succ ln], col <- [pred scol .. succ ecol]]

eligible :: Schematic -> [Int]
eligible Schematic{..} = [read $ part e | e <- eps, nearSym e]
  where
    nearSym pp = any (isJust . flip lookup sym) $ near pp

part1 :: IO ()
part1 = getArgs >>= readFile . head >>= print . sum . eligible . helper2 . helper1

-- mk a list of schematics by lines
helper2 :: [Either ((Int, Int), Char) EnginePart] -> Schematic
helper2 (partitionEithers -> schem) = Schematic (snd schem) (fst schem)

example1 :: String
example1 = "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598.."

nearGears :: Schematic -> [((Int, Int), EnginePart)]
nearGears Schematic{..} = [(rc, p) | p <- eps, rc <- near p, '*' <- maybeToList $ lookup rc sym]

filterGears :: [((Int, Int), EnginePart)] -> [((Int, Int), EnginePart)]
filterGears xs = [(r, p) | (r, p) <- xs, length (filter ((== r) . fst) xs) == 2]

gearRatio :: [((Int, Int), EnginePart)] -> [Int]
gearRatio [] = []
gearRatio (x : xs) =
    let rc = fst x
     in case lookup rc xs of
            Nothing -> gearRatio xs
            Just ep -> read (part ep) * read (part $ snd x) : gearRatio xs

part2 :: IO ()
part2 = getArgs >>= readFile . head >>= print . sum . gearRatio . filterGears . nearGears . helper2 . helper1
