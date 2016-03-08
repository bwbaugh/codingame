{-
Goal
====

The classic game of Connect Four is played by two players in a grid of
6 rows by 7 columns, standing vertically. Players alternately drop one
token from the top of one column, and the token falls to the lowest
possible grid cell, stacking up on the tokens below it. The first
player to make a horizontal, vertical or diagonal line of 4 tokens wins
the game.

From the description of one game grid, your program must determine in
which columns each player may complete a line if they play first on the
next turn.

Input
=====

6 lines: the game grid, given from top to bottom.

`1` and `2` stand for the respective player's token and `.` for an
empty cell.

Output
======

Line 1: the columns that would make player 1 win, sorted and separated
by spaces, or NONE if there are no columns to output.

Line 2: the columns that would make player 2 win, sorted and separated
by spaces, or NONE if there are no columns to output.

Column indices start at 0 for the left column.

Constraints
===========

There is always at least one playable column (i.e. the grid is never
full).

Example
=======

Input
-----

.......
.......
.......
.......
..222..
..111..

Output
------

1 5
NONE
-}
import Control.Monad
import Data.List

type Player = Char
type Grid = [String]

main :: IO ()
main = do
    grid <- replicateM 6 getLine
    forM_ ['1', '2'] $ \player -> do
        let columns = winningMoves player grid
        if null columns
            then putStrLn "NONE"
            else putStrLn . unwords . map show $ columns

winningMoves :: Player -> Grid -> [Int]
winningMoves player grid = do
    column <- [0..length (head grid) - 1]
    -- TODO: Would it be nicer if makeMove returned Maybe Grid?
    guard $ head (transpose grid !! column) == '.'
    guard $ isWinning player $ makeMove player grid column
    return column

-- | Make a move. The column must be playable.
makeMove :: Player -> Grid -> Int -> Grid
makeMove player grid column = transpose $ replaceAtIndex column xs' tGrid
  where
    tGrid = transpose grid
    xs' = concat $ (\(empty:rest) -> init empty : [player] : rest) $ group xs
    xs = tGrid !! column

-- | http://stackoverflow.com/a/10133429/1988505
replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls

isWinning :: Player -> Grid -> Bool
isWinning player grid
    | rowCondition grid = True
    | rowCondition $ transpose grid = True
    | any isSucc diagonals || any (isSucc . reverse) diagonals = True
    | otherwise = False
  where
    rowCondition = any (replicate 4 player `isInfixOf`)
    diagonals =
        concatMap sequence $
        filter ((>= 4) . length) $
        filter (not . null . head) $
        groupBy (\x y -> null x == null y) $
        map (findIndices (== player)) grid

-- | http://stackoverflow.com/a/15542475/1988505
isSucc :: (Enum a, Eq a) => [a] -> Bool
isSucc xs = and $ zipWith ((==) . succ) xs (tail xs)
