module BinaryDiagnosticPartTwo where

import Data.List ( transpose )
import Aoc2021 ( readLines, toDec )

cmp :: (Int -> Int -> Bool) -> [Char] -> Char
cmp f lst
    = let occurrences = length $ filter (== '1') lst
      in if occurrences `f` (length lst - occurrences)
         then '1'
         else '0'

reduceWith :: (Int -> Int -> Bool) -> [String] -> [String]
reduceWith func lst =
    goR func lst 0
    where goR f l n
            | n >= length (head l) = l
            | length l < 2         = l
            | otherwise =
                  goR func (filter ((== target) . (!! n)) l) (n + 1)
                  where transposed = transpose l
                        column = transposed !! n
                        target = cmp f column

collapse :: Foldable t => (t a -> t a) -> t a -> t a
collapse = until (\l -> length l < 2)

solve :: String -> IO ()
solve f = do
    lines <- readLines f
    let oxygen = head $ (collapse $ reduceWith (>=)) lines
    let co2 = head $ (collapse $ reduceWith (<)) lines
    print $ toDec oxygen * toDec co2
