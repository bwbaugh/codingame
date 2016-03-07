module Main where

data Cell = Dead | Live
newtype Grid = Grid [[Cell]]

instance Show Cell where
    show Dead = "0"
    show Live = "1"

instance Show Grid where
    show (Grid xss) = unlines . map (concatMap show) $ xss

main :: IO ()
main = interact $ show . parseGrid . tail . lines

parseGrid :: [String] -> Grid
parseGrid = Grid . map (map parseCell)

parseCell :: Char -> Cell
parseCell '0' = Dead
parseCell '1' = Live
parseCell x = error $ "parseCell: unknown char: " ++ [x]
