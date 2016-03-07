module Main where

data Cell = Dead | Live
type Grid = [[Cell]]

instance Show Cell where
    show Dead = "0"
    show Live = "1"

main :: IO ()
main = interact $ unlines . map (concatMap show) . parseGrid . tail . lines

parseGrid :: [String] -> Grid
parseGrid = map (map parseCell)

parseCell :: Char -> Cell
parseCell '0' = Dead
parseCell '1' = Live
parseCell x = error $ "parseCell: unknown char: " ++ [x]
