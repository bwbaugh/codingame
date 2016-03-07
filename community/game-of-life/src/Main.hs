module Main where

data Cell = Dead | Live deriving (Show)
newtype Grid = Grid [[Cell]]

instance Show Grid where
    show (Grid xss) = unlines . map (map cellToChar) $ xss
      where
        cellToChar Dead = '0'
        cellToChar Live = '1'

main :: IO ()
main = interact $ show . parseGrid . tail . lines

parseGrid :: [String] -> Grid
parseGrid = Grid . map (map parseCell)

parseCell :: Char -> Cell
parseCell '0' = Dead
parseCell '1' = Live
parseCell x = error $ "parseCell: unknown char: " ++ [x]
