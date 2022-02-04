module DivePartOne where

import Data.Either ( rights )
import Text.ParserCombinators.Parsec
    ( parse, digit, spaces, letter, many1, Parser )
import Aoc2021 ( parseRights, readLines )

data Direction =
    Fwd Int
  | Up Int
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

depthF :: Direction -> Int
depthF (Up n)  = negate n
depthF (Dwn n) = n
depthF _     = 0

horizF :: Direction -> Int
horizF (Fwd n) = n
horizF _       = 0

solve :: String -> IO ()
solve f = do
    lines <- readLines f
    let directions = parseRights direction lines
    let depth = foldr (\cur dep -> dep + depthF cur) 0 directions
    let horizontal = foldr (\cur h -> h + horizF cur) 0 directions
    print $ horizontal * depth
