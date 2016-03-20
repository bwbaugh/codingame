module Main where

import Control.Monad
import Data.Char (digitToInt)
import Data.Maybe (catMaybes, fromJust, mapMaybe)
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

validPairs :: Grid -> [Pair]
validPairs grid = filter (checkPair grid) allPairs

allPairs :: [Pair]
allPairs = [(u, v) | u <- allIndices, v <- getAdjacent u]

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
checkPair grid (u, v) =
    any (any (\(a, b, c) -> a == b && a == c))
        (map (getAlignments grid') [u, v])
  where
    grid' = swap grid u v

swap :: Grid -> (Int, Int) -> (Int, Int) -> Grid
swap grid u v = grid U.// [(i1, v2), (i2, v1)]
  where
    [i1, i2] = map (fromJust . toIdx) [u, v]
    [v1, v2] = map (getCell grid) [u, v]

getCell :: Grid -> (Int, Int) -> Int
getCell grid point = grid ! (fromJust . toIdx) point

toIdx :: (Int, Int) -> Maybe Int
toIdx (row, column)
    | row < 0 || column < 0 = Nothing
    | row > 8 || column > 8 = Nothing
    | otherwise = Just (row * 9 + column)

getAlignments :: Grid -> (Int, Int) -> [(Int, Int, Int)]
getAlignments grid (row, column) =
    map (\[a, b, c] -> (a, b, c)) . filter ((== 3) . length) $
        map (mapMaybe (maybe Nothing (grid U.!?) . toIdx))
            [hLeft, hMid, hRight, vTop, vMid, vBot]
  where
    hLeft = [ (row, column - 2)
            , (row, column - 1)
            , (row, column) ]
    hMid = [ (row, column - 1)
           , (row, column)
           , (row, column + 1) ]
    hRight = [ (row, column)
           , (row, column + 1)
           , (row, column + 2) ]
    vTop = [ (row - 2, column)
           , (row - 1, column)
           , (row, column) ]
    vMid = [ (row - 1, column)
           , (row, column)
           , (row + 1, column) ]
    vBot = [ (row, column)
           , (row + 1, column)
           , (row + 2, column) ]
