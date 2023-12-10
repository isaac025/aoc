module Day8 where

import Data.Maybe (fromMaybe)

move ::
    String ->
    (String, (String, String)) ->
    [(String, (String, String))] ->
    Int
move [] _ _ = undefined
move ('L' : m) a@(h, (l, r)) xs =
    if l == "ZZZ"
        then 1
        else
            let nm = fromMaybe (l, r) (lookup h xs)
             in 1 + move m nm (tail xs ++ [a])
