module Main where

import Control.Monad
import Data.Char (digitToInt)
import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as U

type Grid = U.Vector Int
type Pair = ((Int, Int), (Int, Int))

main :: IO ()
main = do
    grid <- readGrid
    let pairs = validPairs grid
    print $ length pairs
    putStr . unlines . map (unwords . map show . pairToList) $ pairs
  where
    pairToList :: Pair -> [Int]
    pairToList ((r1, c1), (r2, c2)) = [r1, c1, r2, c2]

readGrid :: IO Grid
readGrid = liftM parseGrid (replicateM 9 getLine)

parseGrid :: [String] -> Grid
parseGrid = U.fromList . map digitToInt . concat . words . unlines

getCell :: Grid -> (Int, Int) -> Int
getCell grid (row, column) = grid ! idx
  where
    idx = row * 9 + column

validPairs :: Grid -> [Pair]
validPairs grid = filter (checkPair grid) $ genPairs grid

genPairs :: Grid -> [Pair]
genPairs = undefined

checkPair :: Grid -> Pair -> Bool
checkPair = undefined
