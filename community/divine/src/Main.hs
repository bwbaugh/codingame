module Main where

import Control.Monad
import Data.Char (digitToInt)
import Data.Maybe (catMaybes)
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
validPairs grid = filter (checkPair grid) allPairs

allPairs :: [Pair]
allPairs = do
    start <- allIndices
    neighbor <- getAdjacent start
    return (start, neighbor)

allIndices :: [(Int, Int)]
allIndices = map (`divMod` 9) [0..81 - 1]

-- | Only the neighbors to the right and below are returned so that
-- each undirected pair is only considered once and in lexical order.
--
-- For example:
--
-- >>> filter (\(a, b) -> a == (5, 7) || b == (5, 7)) allPairs
-- [((4,7),(5,7)),((5,6),(5,7)),((5,7),(5,8)),((5,7),(6,7))]
getAdjacent :: (Int, Int) -> [(Int, Int)]
getAdjacent (row, column) = catMaybes [right, below]
  where
    right = if column + 1 < 9 then Just (row, column + 1) else Nothing
    below = if row + 1 < 9 then Just (row + 1, column) else Nothing

checkPair :: Grid -> Pair -> Bool
checkPair = undefined
