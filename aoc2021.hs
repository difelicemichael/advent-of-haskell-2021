module Aoc2021 where

-- | Read a file and split it by newlines.
readLines :: FilePath -> IO [String]
readLines f = do
    content <- readFile f
    return $ lines content

-- | Read a file and split it by newlines, 
-- reading each line as a new `Integer`.
readInts :: FilePath -> IO [Integer]
readInts f = do
    lines <- readLines f
    return $ (\s -> read s :: Integer) <$> lines
