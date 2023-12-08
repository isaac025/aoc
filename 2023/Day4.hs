module Day4 where

import Data.List (groupBy, intersect, singleton)
import System.Environment

lengthWinNums :: String -> Int
lengthWinNums xs =
    let xs' = tail $ dropWhile (/= ':') xs
        [x, _, z] = groupBy (\a b -> a /= '|' && b /= '|') (tail xs')
        n = words x `intersect` words z
     in length n

help1 :: Int -> Int
help1 0 = 0
help1 1 = 1
help1 x = 2 ^ (x - 1)

example :: String
example = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

part1 :: IO ()
part1 = getArgs >>= readFile . head >>= print . sum . map (help1 . lengthWinNums) . lines

help2 :: [[Int]] -> [Int]
help2 [x] = [length x]
help2 (x : xs)
    | null x = help2 xs
    | otherwise =
        let v = head x
            l = length x
            nh = updateLs (take v xs) l
            lh = drop v xs
         in l : help2 (nh ++ lh)

updateLs :: [[Int]] -> Int -> [[Int]]
updateLs [] _ = []
updateLs (x : xs) l
    | null x = updateLs xs l
    | otherwise =
        let v = head x
            ln = length x
         in replicate (l + ln) v : updateLs xs l

part2 :: IO ()
part2 = getArgs >>= readFile . head >>= print . sum . help2 . map (singleton . lengthWinNums) . lines
