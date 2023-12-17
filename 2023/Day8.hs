module Day8 where

import Data.Char (isAlphaNum)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

move ::
    String ->
    (String, String) ->
    [(String, (String, String))] ->
    Int ->
    Int
move (x : m) (l, r) xs n
    | x == 'L' && l == "ZZZ" = n
    | x == 'R' && r == "ZZZ" = n
    | x == 'R' =
        let nm = lookup r xs
         in case nm of
                Nothing -> error "looked for something that doesn't exist!"
                Just x -> move (m ++ "R") x xs (n + 1)
    | otherwise =
        let nm = lookup l xs
         in case nm of
                Nothing -> error "looked for something that doesn't exist!"
                Just x -> move (m ++ "L") x xs (n + 1)

parse :: String -> (String, (String, String))
parse xs =
    let v = splitAt 3 $ filter isAlphaNum $ drop 6 xs
     in (take 3 xs, v)

example1 :: String
example1 = "RL\n\nAAA = (BBB, CCC)\nBBB = (DDD, EEE)\nCCC = (ZZZ, GGG)\nDDD = (DDD, DDD)\nEEE = (EEE, EEE)\nGGG = (GGG, GGG)\nZZZ = (ZZZ, ZZZ)"

example2 :: String
example2 = "LLR\n\nAAA = (BBB, BBB)\nBBB = (AAA, ZZZ)\nZZZ = (ZZZ, ZZZ)"

part1 :: IO ()
part1 = do
    args <- getArgs
    c <- lines <$> readFile (head args)
    let x = concat $ take 1 c
        v = map parse (drop 2 c)
    print $ move x (fromMaybe (snd $ head v) (lookup "AAA" v)) v 1
