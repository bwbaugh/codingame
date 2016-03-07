module Main where

import Data.List.Split (chunksOf)

data Cell = Dead | Live deriving (Eq, Show)
newtype Grid = Grid [[Cell]]

instance Show Grid where
    show (Grid xss) = unlines . map (map cellToChar) $ xss
      where
        cellToChar Dead = '0'
        cellToChar Live = '1'

main :: IO ()
main = interact $ show . nextState . parseGrid . tail . lines

parseGrid :: [String] -> Grid
parseGrid = Grid . map (map parseCell)

parseCell :: Char -> Cell
parseCell '0' = Dead
parseCell '1' = Live
parseCell x = error $ "parseCell: unknown char: " ++ [x]

nextState :: Grid -> Grid
nextState grid@(Grid xss) = Grid . chunksOf numCols . map (uncurry applyRule) $ zip (concat xss) neighbors
  where
    neighbors :: [Int]
    neighbors = map (flip countLiveNeighbors grid) indices
    indices :: [(Int, Int)]
    indices = map toPoint . sequence $ [[0..numRows-1], [0..numCols-1]]
    (numRows, numCols) = getDimensions grid

getDimensions :: Grid -> (Int, Int)
getDimensions (Grid xss) = (numRows, numCols)
  where
    numRows = length xss
    numCols = length $ head xss

toPoint :: [Int] -> (Int, Int)
toPoint [r, c] = (r, c)
toPoint _ = error "toPoint non-exhaustive"

countLiveNeighbors :: (Int, Int) -> Grid -> Int
countLiveNeighbors pos grid = length . filter (== Live) $ getNeighbors pos grid

getNeighbors :: (Int, Int) -> Grid -> [Cell]
getNeighbors (r, c) (Grid xss) = map getCell . filter inBounds $ filter (/= (r, c)) toTry
  where
    toTry :: [(Int, Int)]
    toTry = map toPoint . sequence $ [[r-1, r, r+1], [c-1, c, c+1]]
    inBounds (r', c')
        | r' < 0 || c' < 0 || r' >= numRows || c' >= numCols = False
        | otherwise = True
      where
        (numRows, numCols) = getDimensions $ Grid xss
    getCell :: (Int, Int) -> Cell
    getCell (r', c') = xss !! r' !! c'

applyRule :: Cell -> Int -> Cell
applyRule Live 2 = Live
applyRule Live 3 = Live
applyRule Live _ = Dead
applyRule Dead 3 = Live
applyRule Dead _ = Dead
