module TheTreacheryOfWhalesPartOne where

import Aoc2021
    ( splitOn, readInt, readManyWith, diff, maximum', minimum' )
import GHC.Base (maxInt)

minimize :: [Int] -> (Int, Int)
minimize ds =
    foldr fuelRequired (0, maxInt) candidates
    where candidates = [minimum' ds .. maximum' ds]
          fuelRequired pos (runningPos, runningTotal) =
              let totalFuel = foldr (\cur acc -> acc + diff cur pos) 0 ds
              in  if totalFuel < runningTotal
                  then (pos, totalFuel)
                  else (runningPos, runningTotal)

solve :: FilePath -> IO ()
solve f = do
    distances <- readManyWith f ((readInt <$>) . splitOn ",")
    print $ show (minimize distances)
