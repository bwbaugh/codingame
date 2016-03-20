module Main where

import Control.Monad
import Data.Char (digitToInt)
import qualified Data.Vector.Unboxed as U

type Grid = U.Vector Int

main :: IO ()
main = do
    _ <- readGrid
    putStrLn "0"

readGrid :: IO Grid
readGrid = liftM parseGrid (replicateM 9 getLine)

parseGrid :: [String] -> Grid
parseGrid = U.fromList . map digitToInt . concat . words . unlines
