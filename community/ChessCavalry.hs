{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE TupleSections #-}
import Control.Monad
import Data.List (findIndex, minimumBy)
import Data.Ord (comparing)

type Board = [[Square]]

type Square = Maybe Piece

data Piece = Blocked | Begin | End deriving (Eq, Show)

type Point = (Int, Int)

main :: IO ()
main = do
    board <- readBoard
    case solve board [] (find (Just Begin) board) (find (Just End) board) of
        [] -> putStrLn "Impossible"
        xss -> print . subtract 1 . length . minimumBy (comparing length) $ xss

readBoard :: IO Board
readBoard = do
    [_, h] <- fmap (map read . words) getLine :: IO [Int]
    replicateM h $ fmap (map readSquare) getLine

readSquare :: Char -> Square
readSquare '.' = Nothing
readSquare '#' = Just Blocked
readSquare 'B' = Just Begin
readSquare 'E' = Just End
readSquare c = error $ "Unknown square: " ++ [c]

find :: Square -> Board -> Point
find square board = go 0
  where
    go row = maybe (go (row + 1)) (row,) $
        findIndex (square ==) (board !! row)

solve :: Board -> [Point] -> Point -> Point -> [[Point]]
solve board [] start end = solve board [start] start end
solve _ path _ end | last path == end = [[]]
solve board path start end = do
    point <- legalMoves board (last path)
    guard (point `notElem` path)
    let path' = path ++ [point]
    suffix <- solve board path' start end
    return $ path' ++ suffix

legalMoves :: Board -> Point -> [Point]
legalMoves board (row, col) = do
    point <- [
          (row - 2, col + 1)
        , (row - 1, col + 2)
        , (row + 1, col + 2)
        , (row + 2, col + 1)
        , (row + 2, col - 1)
        , (row + 1, col - 2)
        , (row - 1, col - 2)
        , (row - 2, col - 1)
        ]
    guard (inBounds board point)
    guard (board !! row !! col /= Just Blocked)
    return point

inBounds :: Board -> Point -> Bool
inBounds board (row, col)
    | row < 0 || row >= length board = False
    | col < 0 || col >= length (head board) = False
    | otherwise = True
