module HydrodermalVenture where
import Aoc2021
import qualified Data.Map as M

isLine :: (Eq a1, Eq a2) => ((a1, a2), (a1, a2)) -> Bool
isLine ((x0, y0), (x1, y1)) =
    x0 == x1 || y0 == y1

getSegment :: String -> ((Int, Int), (Int, Int))
getSegment s =
    tuplify2 (asInts <$> split)
    where split  = splitOn "->" s
          asInts = tuplify2 . (readInt <$>) . splitOn ","

-- | Simple list comprehensions, unless we are constructing a
-- diagonal line of points, in which case we first determine
-- the (x, y) pair containing the minimum x, and then construct
-- a range with either increasing or decreasing y based on 
-- x & y values.
lineVertices :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
lineVertices ((x0, y0), (x1, y1))
    | x0 == x1  = [(x0, y) | y <- [min y0 y1 .. max y0 y1]]
    | y0 == y1  = [(x, y0) | x <- [min x0 x1 .. max x0 x1]]
    | otherwise =
        [(n + fst startXY, snd startXY `op` n) | n <- [0..max x0 x1 - min x0 x1]]
        where startXY =  if x0 < x1 then (x0, y0) else (x1, y1)
              op
                | x0 < x1 && y0 < y1 = (+)
                | x0 > x1 && y0 > y1 = (+)
                | otherwise          = (-)

solveWithFilter :: [String] -> (((Int, Int), (Int, Int)) -> Bool) -> Int
solveWithFilter lst r
    = let segments = filter r $ getSegment <$> lst
          vertices = concat $ lineVertices <$> segments
          maps     = M.partition (> 1) (countMap vertices)
      in  length (M.keys $ fst maps)

solve :: [String] -> Int
solve lst = solveWithFilter lst (const True)

solveA :: FilePath -> IO ()
solveA f = do
    lines <- readLines f
    print $ solveWithFilter lines isLine

solveB :: FilePath -> IO ()
solveB f = do
    lines <- readLines f
    print $ solve lines
