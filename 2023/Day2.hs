{-# LANGUAGE RecordWildCards #-}

module Day2 where

import Data.Bool (bool)
import Data.Maybe (isNothing)
import System.Environment (getArgs)

data Game = Game
    { gameId :: Int
    , configuration :: [(String, Int)]
    }
    deriving (Show)

sumIds :: [Game] -> Int
sumIds = sum . map gameId

filterGame :: Game -> Bool
filterGame Game{..} =
    let reds = (<= 12) <$> lookup "red" configuration
        greens = (<= 13) <$> lookup "green" configuration
        blues = (<= 14) <$> lookup "blue" configuration
     in and reds && and greens && and blues

mkMap :: [String] -> [(String, Int)] -> [(String, Int)]
mkMap [] xs = xs
mkMap (x : y : ss) xs =
    if isNothing (lookup y xs)
        then mkMap ss ((y, read x) : xs)
        else mkMap ss [if y == r then (r, bool c (read x) (c <= read x)) else (r, c) | (r, c) <- xs]

constructGame :: String -> Game
constructGame xs =
    let (i : ss) = tail $ words $ sanitize xs
        conf = mkMap ss []
     in Game{gameId = read i, configuration = conf}
  where
    sanitize = filter (not . (`elem` ",;:"))

constructGames :: String -> [Game]
constructGames = map constructGame . lines

example1 :: String
example1 = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

-- part 2
mkMap' :: [String] -> [(String, Int)] -> [(String, Int)]
mkMap' [] xs = xs
mkMap' (x : y : ss) xs =
    if isNothing (lookup y xs)
        then mkMap ss ((y, read x) : xs)
        else mkMap ss [if y == r then (r, bool c (read x) (read x <= c)) else (r, c) | (r, c) <- xs]

constructGame' :: String -> Game
constructGame' xs =
    let (i : ss) = tail $ words $ sanitize xs
        conf = mkMap' ss []
     in Game{gameId = read i, configuration = conf}
  where
    sanitize = filter (not . (`elem` ",;:"))

constructGames' :: String -> [Game]
constructGames' = map constructGame' . lines

power :: Game -> Int
power = product . map snd . configuration

main :: IO ()
main = do
    args <- getArgs
    case args of
        [f] -> do
            c <- readFile f
            print $ sumIds $ filter filterGame $ constructGames c
            print $ sum $ map power $ constructGames' c
        _ -> error "Expected one file"
