{-# LANGUAGE RecordWildCards #-}

module Day3 where

import Data.Char
import Prelude hiding (head, init, last, tail)

class Deque q where
    empty :: q a
    isEmpty :: q a -> Bool
    cons :: a -> q a -> q a
    head :: q a -> a
    tail :: q a -> q a
    snoc :: q a -> a -> q a
    last :: q a -> a
    init :: q a -> q a

data DList a = DL Int [a] Int [a] deriving (Show)

checkf :: Int -> [a] -> Int -> [a] -> DList a
checkf lenf f lenr r
    | lenf > c * lenr + 1 =
        let i = (lenf + lenr) `div` 2
            j = lenf + lenr - i
            f' = take i f
            r' = r ++ reverse (drop i f)
         in DL i f' j r'
    | lenr > c * lenf + 1 =
        let j = (lenf + lenr) `div` 2
            i = lenf + lenr - j
            r' = take j r
            f' = f ++ reverse (drop j r)
         in DL i f' j r'
    | otherwise = DL lenf f lenr r
  where
    c = 3

instance Deque DList where
    empty = DL 0 [] 0 []
    isEmpty (DL lenf _ lenr _) = lenf + lenr == 0
    cons x (DL lenf f lenr r) = checkf (lenf + 1) (x : f) lenr r
    head (DL _ [] _ _) = error "empty deque"
    head (DL _ (x : _) _ _) = x
    tail (DL _ [] _ _) = error "empty deque"
    tail (DL lenf (_ : f) lenr r) = checkf (lenf - 1) f lenr r
    snoc (DL lenf f lenr r) x = checkf lenf f (lenr + 1) (x : r)
    last (DL _ _ _ []) = error "empty deque"
    last (DL _ _ _ (x : _)) = x
    init (DL _ _ _ []) = error "empty deque"
    init (DL lenf f lenr (x : r)) = checkf lenf f (lenr - 1) r

fromList :: [a] -> DList a
fromList = foldr cons empty

-- representation of values
data EnginePart = EnginePart
    { part :: String
    , idx :: [Int]
    }
    deriving (Show)

type Schematic = [EnginePart]

-- mk a tuple of number/symbol w indexes
helper1 :: String -> [EnginePart]
helper1 xs = insideH xs 0
  where
    insideH [] _ = []
    insideH (x : xs) y
        | x == '.' = insideH xs (y + 1)
        | isDigit x =
            let (part, left) = (x : takeWhile isDigit xs, dropWhile isDigit xs)
                idx = [y .. y + length part - 1]
             in EnginePart{..} : insideH left (y + length part)
        | otherwise = EnginePart [x] [y] : insideH xs (y + 1)

-- mk a list of schematics by lines
helper2 :: String -> DList Schematic
helper2 = fromList . map helper1 . lines

-- check if index list are in bounds
check :: [Int] -> [Int] -> Bool
check xs ys = (not . null) [x | x <- xs, y <- ys, let v = x - y in v >= -1 && v <= 1]

example1 :: String
example1 = "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598.."
