{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE TupleSections #-}
import Control.Monad
import Data.List (findIndex)

type Board = [[Square]]

type Square = Maybe Piece

data Piece = Blocked | Begin | End deriving (Eq, Show)

type Point = (Int, Int)

main :: IO ()
main = do
    board <- readBoard
    case solve board [[find (Just Begin) board]] (find (Just End) board) of
        [] -> putStrLn "Impossible"
        path -> print . subtract 1 . length $ path

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

solve :: Board -> [[Point]] -> Point -> [Point]
solve _ [] _ = []
solve board (parent:queue) end
    | end `elem` moves = parent ++ [end]
    | otherwise = solve board (queue ++ children) end
  where
    moves = filter (`notElem` parent) $ legalMoves board (last parent)
    children = map ((parent ++) . (:[])) moves

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
