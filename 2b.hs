module DivePartTwo where

import Aoc2021 ( parseRights, readLines )
import Data.List ( foldl' )
import Text.ParserCombinators.Parsec
    ( parse, digit, spaces, letter, many1, Parser )

data Direction =  Fwd Int
                | Up  Int
                | Dwn Int
                | Nop
                deriving (Eq, Show)

direction :: Parser Direction
direction = do
  instr <- many1 letter
  spaces
  nmber <- many1 digit
  return $ case instr of
    "forward" -> Fwd $ read nmber
    "down"    -> Dwn $ read nmber
    "up"      -> Up $ read nmber
    _         -> Nop

calc :: (Int, Int, Int) -> Direction -> (Int, Int, Int)
calc (pos, dep, aim) dir = case dir of
                             Up  n -> (pos, dep, aim - n)
                             Dwn n -> (pos, dep, aim + n)
                             Fwd n -> (pos + n, dep + (aim * n), aim)
                             _     -> (pos, dep, aim)

solve :: String -> IO ()
solve f = do
    lines <- readLines f
    let directions = parseRights direction lines
    let (pos, dep, _) = foldl' calc (0, 0, 0) directions
    print $ pos * dep
