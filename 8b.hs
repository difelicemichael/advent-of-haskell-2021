module SevenSegmentSearchPartTwo where

import Aoc2021
import Data.Maybe (isJust, fromJust, isNothing)
import Data.List (sort, foldl', (\\), union)
import Data.Tuple

import qualified Data.Map as M

-- | Number of lines per digit, sorted by digit.
--   "1" = 2
--   "7" = 3
--   "4" = 4
--   "2" = 5 | "3" = 5 | "5" = 5
--   "0" = 6 | "6" = 6 | "9" = 5
--   "8" = 7
--
-- Rules for developing a string map.
-- "1", "4", "7", and "8" can be derived from the number of distinct characters.
-- "3" \\ "7" yields three segments, while the other 5-length segment digits yield more segments.
-- "2" \\ "4" yields three segments, while "5" \\ "4" yields only two.
-- "0" can be determined by (\\ 5), "6" and "9" will differ by one segment and 0 will differ by two.
-- "9" \\ "1" is length 4.
-- "6" \\ "1" is length 5.
reveal :: [String] -> Int -> M.Map Int String -> Maybe String
reveal groups 3 m = do
    seven <- M.lookup 1 m
    let length5 = groups `ofLength` 5
    let f = \s -> length (s \\ seven) == 3
    return $ single (filter f length5)
reveal groups 2 m = do
    four <- M.lookup 4 m
    three <- M.lookup 3 m
    let length5 = groups `ofLength` 5
    let twoOrFive = filter (not . equals three) length5
    let f = \s -> length (s \\ four) == 3
    return $ single (filter f twoOrFive)
reveal groups 5 m = do
    two <- M.lookup 2 m
    three <- M.lookup 3 m
    let coll = [two, three]
    let length5 = groups `ofLength` 5
    return $ single (filter (`notElem` coll) length5)
reveal groups 0 m = do
    five <- M.lookup 5 m
    let length6 = groups `ofLength` 6
    let f = \s -> length (s \\ five) == 2
    return $ single (filter f length6)
reveal groups 6 m = do
    one <- M.lookup 1 m
    zero <- M.lookup 0 m
    let length6Not0 = filter (not . equals zero) (groups `ofLength` 6)
    let f = \s -> length (s \\ one) == 4
    return $ single (filter f length6Not0)
reveal groups 9 m = do
    one <- M.lookup 1 m
    let length6 = groups `ofLength` 6
    let f = \s -> length (s \\ one) == 5
    return $ single (filter f length6)
reveal _ _ _ = error "Unknown digit."

getStringMap :: [String] -> M.Map Int String
getStringMap strs =
    reduceMap strs M.empty
    where reduceMap [] _ = M.empty
          reduceMap (x:xs) m
            -- success condition.
            | all (`M.member` m) [0..10] = m
            -- if we are already a member
            | isJust $ group `lookupKey` m = reduceMap xs m
            -- attempt distinct element identification.
            | isJust $ identify group = reduceMap xs (M.insert (fromJust $ identify group) group m)
            -- otherwise, attempt the reveal, based on the prioritized reveal list.
            | otherwise         = reduceMap xs (revealRound groups)
            where group         = sort (distinct x)
                  groups        = sort . distinct <$> strs
                  revealRound gs = foldl' (flip (tryReveal gs)) m [9, 3, 5, 2, 6, 0]
                  tryReveal gs n m = if n `M.member` m then m
                                     else case reveal gs n m of
                                            Just s  -> M.insert n s m
                                            Nothing -> m

identify :: String -> Maybe Int
identify s =
    case length $ distinct s of
        2 -> Just 1
        4 -> Just 4
        3 -> Just 7
        7 -> Just 8
        _ -> Nothing

resolveSingle :: M.Map String Int -> [String] -> Int
resolveSingle m digits =
    fst $ foldl' (\(acc, p) d -> (acc + (d * p), p * 10)) (0, 1) resolvedDs
    where resolvedDs = r m <$> digits
          r m d = case M.lookup d m of
                    Just k -> k
                    Nothing -> error "could not map digit to key."

solve :: FilePath -> IO ()
solve f = do
    lines <- readLinesWith f (splitOn "|")
    let inputs = (\j -> (words $ head j, words $ last j)) <$> lines
    let mapping = M.fromList . (swap <$>) . M.toList . getStringMap
    let r = (\i -> resolveSingle (mapping $ fst <$> i) (snd <$> i))
    undefined
