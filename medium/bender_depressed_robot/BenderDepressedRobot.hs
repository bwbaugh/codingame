import Control.Arrow (second)
import Data.List
import Data.Maybe
import System.IO

data Direction = SOUTH | EAST | NORTH | WEST
    deriving (Eq, Ord, Show, Enum)

type Cell = Char
type Grid = [[Cell]]
type Position = (Int, Int)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    _ <- getLine
    grid <- fmap lines getContents
    -- TODO(bwbaugh|2016-01-30): Handle grids resulting in "LOOP".
    let path = genPath grid (initialPosition grid) initialDirection
    putStr $ unlines $ map show path

initialPosition :: Grid -> Position
initialPosition grid = (rowIndex, columnIndex)
    where
    (rowIndex, columnIndex) =
        fromMaybe (error "no row with start location") $ listToMaybe $
        map (second fromJust) $ filter (isJust . snd) $
        zip [0..] (map (elemIndex '@') grid)

initialDirection :: Direction
initialDirection = SOUTH

genPath :: Grid -> Position -> Direction -> [Direction]
genPath grid position direction
    | nextCell == '$' = [direction]
    -- TODO(bwbaugh|2016-01-30): Properly handle obstacles as `succ` is
    --   not the proper way. Instead we should be trying all directions
    --   starting with SOUTH.
    -- TODO(bwbaugh|2016-01-30): Handle reverse order when inverted.
    | nextCell `elem` "X#" = genPath grid position (succ direction)
    | nextCell == 'N' = direction : genPath grid position' NORTH
    | nextCell == 'S' = direction : genPath grid position' SOUTH
    | nextCell == 'E' = direction : genPath grid position' EAST
    | nextCell == 'W' = direction : genPath grid position' WEST
    -- TODO(bwbaugh|2016-01-30): Handle circuit inverters 'I',
    --   Breaker mode 'B', and teleporters 'T'.
    | otherwise = direction : genPath grid position' direction
    where
    nextCell = getCell grid position'
    position' = nextPos direction position

nextPos :: Direction -> Position -> Position
-- TODO(bwbaugh|2016-01-30): Handle out of bounds (return a Maybe Position).
nextPos NORTH (row, column) = (row - 1, column)
nextPos SOUTH (row, column) = (row + 1, column)
nextPos EAST (row, column) = (row, column + 1)
nextPos WEST (row, column) = (row, column - 1)

getCell :: Grid -> Position -> Cell
getCell grid (row, column) = grid !! row !! column
