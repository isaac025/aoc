module Day1 where

import Data.Bool (bool)
import Data.Char (isDigit)
import Data.Maybe (fromJust, isNothing)
import System.Environment (getArgs)

-- Part 1
example1 :: String
example1 = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"

helper1 :: String -> Int
helper1 x =
    let h = head x
        l = last x
     in read (h : [l])

part1 :: String -> Int
part1 = sum . map (helper1 . filter isDigit) . lines

-- Part 2
example2 :: String
example2 = "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen"

numberMap :: [(String, Int)]
numberMap =
    [ ("one", 1)
    , ("two", 2)
    , ("three", 3)
    , ("four", 4)
    , ("five", 5)
    , ("six", 6)
    , ("seven", 7)
    , ("eight", 8)
    , ("nine", 9)
    , ("zero", 0)
    ]

helper2 :: String -> String
helper2 [] = []
helper2 a@(x : xs)
    | isDigit x = x : helper2 xs
    | x == 'o' = maybe "" show (lookup (take 3 a) numberMap) ++ helper2 xs
    | x == 't' =
        case lookup (take 3 a) numberMap of
            Nothing -> maybe "" show (lookup (take 5 a) numberMap) ++ helper2 xs
            Just x -> show x ++ helper2 xs
    | x == 'f' =
        case lookup (take 4 a) numberMap of
            Nothing -> helper2 xs
            Just x -> show x ++ helper2 xs
    | x == 's' =
        case lookup (take 3 a) numberMap of
            Nothing -> maybe "" show (lookup (take 5 a) numberMap) ++ helper2 xs
            Just x -> show x ++ helper2 xs
    | x == 'e' = maybe "" show (lookup (take 5 a) numberMap) ++ helper2 xs
    | x == 'n' = maybe "" show (lookup (take 4 a) numberMap) ++ helper2 xs
    | x == 'z' = maybe "" show (lookup (take 4 a) numberMap) ++ helper2 xs
    | otherwise = helper2 xs

part2 :: String -> Int
part2 = sum . map (helper1 . helper2) . lines

main :: IO ()
main = do
    args <- getArgs
    case args of
        [f] -> readFile f >>= print . part2
        _ -> error "Just 1 file"
