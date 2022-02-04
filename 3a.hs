module BinaryDiagnosticPartOne where

import Data.List ( transpose )
import Aoc2021 ( readLines, toDec )

cmp :: (Int -> Int -> Bool) -> [Char] -> Char
cmp f lst
    = let occurrences = length $ filter (== '1') lst
      in if occurrences `f` (length lst - occurrences)
         then '1'
         else '0'

solve :: String -> IO ()
solve f = do
    lines <- readLines f
    let transposed = transpose lines
    let gam = toDec $ cmp (>) <$> transposed
    let eps = toDec $ cmp (<) <$> transposed
    print (gam * eps)
