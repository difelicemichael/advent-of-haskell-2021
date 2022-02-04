module GiantSquid where

import Aoc2021 ( chunks, trim, readInt, readLines, first )
import Data.List ( transpose )
import qualified Data.Text as T
import Data.Maybe ( isJust, fromMaybe )

score :: [[Int]] -> [Int] -> Int
score [] _ = 0
score _ [] = 0
score board nums =
  last nums * foldr sumUnmarked 0 board
  where
    sumUnmarked row acc =
      sum $ acc : filter (`notElem` nums) row

bingo :: [[Int]] -> [Int] -> Bool
bingo [] _ = False
bingo _ [] = False
bingo board nums =
  row || column
  where row = success board
        column = success (transpose board)
        success = any (all (`elem` nums))

findWinningBoards :: [Int] -> [[[Int]]] -> Int -> [([[Int]], [Int])]
findWinningBoards numbers boards n =
  go numbers boards n []
  where go nums bs n ws
          | n >= length nums         = ws
          | isJust winningBoard      = go nums (filter (not . bingoBoards) bs) (n + 1) (ws ++ [boardTuple])
          | otherwise                = go nums bs (n + 1) ws
            where bingoBoards  = (`bingo` drawnNumbers)
                  winningBoard = first bingoBoards bs
                  drawnNumbers = take n nums
                  boardTuple   = (fromMaybe [[]] winningBoard, drawnNumbers)

solve :: String -> IO ()
solve f = do
    lines <- readLines f
    let allNums = readInt . T.unpack <$> T.splitOn (T.pack ",") (T.pack $ head lines)
    let intRows = (readInt <$>) . words <$> filter (not . null) (trim <$> drop 1 lines)
    let boards =  chunks 5 intRows
    let winningBoards = findWinningBoards allNums boards 5
    print $ "first board=" ++ show (uncurry score (head winningBoards))
    print $ "last board=" ++ show (uncurry score (last winningBoards))
