module Day6 where

import System.Environment (getArgs)

part1 :: IO ()
part1 = getArgs >>= readFile . head >>= print . product . (\x -> zipWith waysToWin (head x) (last x)) . map (map read . tail . words) . lines

waysToWin :: Int -> Int -> Int
waysToWin t d = length $ [x | x <- [1 .. t], x * (t - x) > d]

example :: String
example = "Time:      7  15   30\nDistance:  9  40  200"

part2 :: IO ()
part2 = getArgs >>= readFile . head >>= print . (\(x, y) -> 1 + (x - y)) . getBounds . (\x -> (head x, last x)) . map (read . concat . tail . words) . lines

getBounds :: (Int, Int) -> (Int, Int)
getBounds (t, d) = (ceiling (t2 - 1), floor (t1 + 1))
  where
    delta = t * t - 4 * d
    root = sqrt (fromIntegral delta)
    (t1, t2) = ((fromIntegral t - root) / 2, (fromIntegral t + root) / 2)
