module SonarSweepPartOne where

import Aoc2021 ( readInts )
import Data.List ( foldl' )

solve :: String -> IO ()
solve f = do
    ints <- readInts f
    print $ snd (foldl' increasing (head ints, 0) ints)
    where
        increasing (prev, acc) cur
            | cur > prev = (cur, acc+1)
            | otherwise  = (cur, acc)
