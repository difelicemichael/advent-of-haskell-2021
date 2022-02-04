module SevenSegmentSearchPartOne where

import Aoc2021
import Data.Maybe (isJust)

identify :: String -> Maybe Int
identify s =
    case length $ distinct s of
        2 -> Just 1
        4 -> Just 4
        3 -> Just 7
        7 -> Just 8
        _ -> Nothing

solve :: FilePath -> IO ()
solve f = do
    lines <- readLinesWith f (splitOn "|")
    let inputs = (\j -> (head j, last j)) <$> lines
    let outputs = concat (words . snd <$> inputs)
    let identified = identify <$> outputs
    print $ length (filter isJust identified)
