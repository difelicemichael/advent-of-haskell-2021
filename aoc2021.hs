module Aoc2021 where

import Text.ParserCombinators.Parsec
import Data.List
import Data.Either
import Control.Monad
import Data.Char

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S

-- | Split a list into chunks of `n` elements.
chunks :: Int -> [a] -> [[a]]
chunks n = takeWhile ( not . null ) . map ( take n ) . iterate ( drop n )

-- | Trim the beginning and end of a
-- `String` for whitespace.
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

-- | Read a file and split it by newlines.
readLines :: FilePath -> IO [String]
readLines f = do
    content <- readFile f
    return $ filter (not . null) (lines content)

-- | Read a file and apply `f` to each line.
readLinesWith :: FilePath -> (String -> a) -> IO [a]
readLinesWith file func = do
    content <- readFile file
    return $ func <$> lines content

-- | Read a file, apply `f` to each line, and
-- `concat` the results.
readManyWith :: FilePath -> (String -> [a]) -> IO [a]
readManyWith file func = do
    content <- readFile file
    return $ concat (func <$> lines content)

-- | Read a file and split it by newlines, 
-- reading each line as a new `Integer`.
readInts :: FilePath -> IO [Integer]
readInts f = do
    lines <- readLines f
    return $ (\s -> read s :: Integer) <$> lines

-- | Read a `String` as an `Int`.
readInt :: String -> Int
readInt = read

-- | Attempt to parse a `String` list and
-- only return the successful results.
parseRights :: Parser a -> [String] -> [a]
parseRights f lst = rights (parse f "" <$> lst)

-- | Converts a character array of 1s and 0s
-- to its base10 `Int` value.
toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

-- | Parse end of line or end of file.
eol :: Parser ()
eol = void (char '\n') <|> eof

-- | Find the first element which satisfies
-- `pred` in the list.
first :: (a -> Bool) -> [a] -> Maybe a
first _ [] = Nothing
first pred (x:xs)
    | pred x    = Just x
    | otherwise = first pred xs

-- | Split and handle packing and unpacking
-- `String`s as encoded `Text`.
splitOn :: String -> String -> [String]
splitOn spl str =
    T.unpack <$> T.splitOn (T.pack spl) (T.pack str)

-- | Relatively unsafe, converts a list of `a` into
-- a two-tuple.
tuplify2 :: [a] -> (a, a)
tuplify2 [] = error "Invalid empty tuple."
tuplify2 xs
    | length xs /= 2 = error "Invalid tuple length."
    | otherwise      = (head xs, last xs)

-- | Count the occurrences of an element within a list.
count:: Eq a => a -> [a] -> Int
count n = foldr (\x -> if n == x then (+1) else id) 0

-- | Return all disinct elements from a list,
-- leveraging `Data.Map` to remove distinct
-- elements.
distinct :: Ord k => [k] -> [k]
distinct s =
    M.keys $ M.fromList $ zip s (enumFrom 0)

-- | Maps pairs to of elements and their
-- frequencies within the provided list
-- to a `Map`.
countMap :: Ord a => [a] -> M.Map a Int
countMap =
    foldr mapFunc M.empty
    where
        mapFunc e m = case M.lookup e m of
                        Just x  -> M.insert e (succ x) m
                        Nothing -> M.insert e 1 m

-- | Find the absolute difference between two orderable
-- elements.
diff :: (Num a, Ord a) => a -> a -> a
diff x y = max x y - min x y

-- | Find the maximum element in a list.
maximum' :: Ord a => [a] -> a
maximum' = foldr1 (\x y ->if x >= y then x else y)

-- | Find the minimum element in a list.
minimum' :: Ord a => [a] -> a
minimum' = foldr1 (\x y ->if x <= y then x else y)

-- | Set equality, sorts the two lists and then
-- determines if they are equal.
equals :: Ord a => [a] -> [a] -> Bool
equals s1 s2
    = equalLength && equalElems
    where equalLength = length s1 == length s2
          equalElems  = all (== True) $ zipWith (==) (sort s1) (sort s2)

-- | Returns `Nothing` on `[]`, else returns
-- the `Just` list.
listToMaybe' :: [a] -> Maybe [a]
listToMaybe' [] = Nothing
listToMaybe' xs = Just xs

-- | Searches for a value within a `Map` and returns
-- the `Just` results in a list if found, and `Nothing`
-- if not values were found.
lookupKey :: Eq a => a -> M.Map p a -> Maybe [a]
lookupKey val m = listToMaybe' (M.foldrWithKey go [] m)
    where
        go key value found =
            if value == val
            then val:found
            else found

-- | Unsafe `head`, enforcing only a single element
-- and throwing an error when not true.
single :: Show a => [a] -> a
single [x] = x
single [] = error "Empty list."
single lst@(x:xs) = error ("List contains multiple elements." ++ show lst)

-- | Unsafe `head`, enforcing only a single element
-- and throwing an error when not true.
ofLength :: Foldable t => [t a] -> Int -> [t a]
ofLength xs n = filter ((== n) . length) xs
