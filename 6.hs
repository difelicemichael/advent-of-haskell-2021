module Lanternfish where

import Aoc2021 ( readLines, readInt, splitOn, countMap )

import Data.List ( foldl', sort )
import qualified Data.Map as M

nextRound :: (Ord a, Num a, Num k, Ord k) => M.Map k a -> M.Map k a
nextRound m =
    foldl' roundFunc M.empty (sort $ M.toList m)
    where
        roundFunc m0 (k, v)
          | k == 0    = M.insert 6 v (M.insert 8 v m0)
          | k == 7    = addTo 6 v m0
          | otherwise = M.insert (k-1) v m0
        addTo k v m1 = case M.lookup k m1 of
                         Just n  -> M.insert k (v+n) m1
                         Nothing -> M.insert k v m1

solve :: FilePath -> IO ()
solve f = do
    lines <- readLines f
    let counters = readInt <$> concat (splitOn "," <$> lines)
    let rounds = iterate nextRound (countMap counters)
    let sumFish gen = foldr (\t acc -> acc + snd t) 0 (M.toList $ rounds !! gen)
    print $ sumFish 80
    print $ sumFish 256
