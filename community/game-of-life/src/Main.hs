module Main where

data Cell = Dead | Live

instance Show Cell where
    show Dead = "0"
    show Live = "1"

main :: IO ()
main = interact $ unlines . map (concatMap (show . parseGrid)) . tail . lines

parseGrid :: Char -> Cell
parseGrid '0' = Dead
parseGrid '1' = Live
