module Day1 where

import Data.Char (isDigit)
import Data.Maybe (isNothing)
import System.Environment (getArgs)

-- Part 1
example1 :: String
example1 = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"

helper :: String -> Int
helper x =
    let h = head x
        l = last x
     in read (h : [l])

day1 :: String -> Int
day1 = sum . map (helper . filter isDigit) . lines

-- Part 2
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

helper1 :: String -> Maybe Int
helper1 [] = Nothing
helper1 a@(x : xs)
    | x == 'o' = lookup (take 3 a) numberMap
    | x == 't' =
        case lookup (take 3 a) numberMap of
            Nothing -> lookup (take 5 a) numberMap
            Just x -> Just x
    | x == 'f' = if isNothing (lookup (take 4 a) numberMap) then helper1 xs else lookup (take 4 a) numberMap
    | x == 's' =
        case lookup (take 3 a) numberMap of
            Nothing -> lookup (take 5 a) numberMap
            Just x -> Just x
    | x == 'e' = lookup (take 5 a) numberMap
    | x == 'n' = lookup (take 4 a) numberMap
    | otherwise = helper1 xs

example2 :: String
example2 = "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [f] -> readFile f >>= print . day1
        _ -> error "Just 1 file"
