module SonarSweepPartTwo where

import Aoc2021 ( readInts )
import Data.List ( foldl' )

solve :: String -> IO ()
solve f = do
    ints <- readInts f
    let lists = groupN 3 ints
    print $ snd (foldl' increasing (sum (head lists), 0) lists)
    where
        increasing (prev, acc) cur
            | sum cur > prev = (sum cur, acc+1)
            | otherwise      = (sum cur, acc)

groupN :: Int -> [a] -> [[a]]
groupN _ [] = []
groupN n lst
    | n >= length lst = [lst]
    | n > 0 = take n lst : groupN n (tail lst)
    | otherwise = error "Invalid group range."
