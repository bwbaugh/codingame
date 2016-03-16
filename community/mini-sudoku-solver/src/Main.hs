{-
You must write a program that solves a 4×4 sudoku grid that have only
one solution. A solved sudoku is a latin square for which each 2×2
corner square must also contain all the digits 1 to 4.

INPUT:
You are given a 4×4 grid, the empty cells are represented by 0. For example:
2000
0130
3001
0240

OUTPUT:
The output must be the only solution, all the zeros must be filled by the correct digits.
Here:
2314
4132
3421
1243

CONSTRAINTS:
There are six hints or more.

EXAMPLE:
Input
2043
0020
4300
0034
Output
2143
3421
4312
1234
-}
module Main where

import Control.Monad
import Data.Char (digitToInt)

type Grid = [[Int]]

main :: IO ()
main = do
    grid <- readGrid
    putStr $ showGrid grid

readGrid :: IO Grid
readGrid = replicateM 4 getLine >>= return . parseGrid

parseGrid :: [String] -> Grid
parseGrid = map (map digitToInt)

showGrid :: Grid -> String
showGrid = unlines . map (concatMap show)
