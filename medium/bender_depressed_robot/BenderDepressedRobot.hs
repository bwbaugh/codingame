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
    -- TODO(bwbaugh|2016-01-31): Consider including direction check.
    let (points, directions) =
            unzip $ genPath grid (initialPosition grid) initialDirection
    if containsDuplicate points then
        putStrLn "LOOP"
    else
        putStr $ unlines $ map show directions

initialPosition :: Grid -> Position
initialPosition grid = (rowIndex, columnIndex)
    where
    (rowIndex, columnIndex) =
        fromMaybe (error "no row with start location") $ listToMaybe $
        map (second fromJust) $ filter (isJust . snd) $
        zip [0..] (map (elemIndex '@') grid)

initialDirection :: Direction
initialDirection = SOUTH

genPath :: Grid -> Position -> Direction -> [(Position, Direction)]
genPath grid position direction
    | nextCell == '$' = [(position, direction)]
    -- TODO(bwbaugh|2016-01-30): Properly handle obstacles as `succ` is
    --   not the proper way. Instead we should be trying all directions
    --   starting with SOUTH.
    -- TODO(bwbaugh|2016-01-30): Handle reverse order when inverted.
    | nextCell `elem` "X#" = genPath grid position (succ direction)
    | nextCell == 'N' = (position, direction) : genPath grid position' NORTH
    | nextCell == 'S' = (position, direction) : genPath grid position' SOUTH
    | nextCell == 'E' = (position, direction) : genPath grid position' EAST
    | nextCell == 'W' = (position, direction) : genPath grid position' WEST
    -- TODO(bwbaugh|2016-01-30): Handle circuit inverters 'I',
    --   Breaker mode 'B', and teleporters 'T'.
    | otherwise = (position, direction) : genPath grid position' direction
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

-- TODO(bwbaugh|2016-01-30): Speed up by using a set or similar.
containsDuplicate :: (Eq a) => [a] -> Bool
containsDuplicate xs = check xs []
    where
    check :: (Eq a) => [a] -> [a] -> Bool
    check [] _ = False
    check (y:ys) seen = y `elem` seen || check ys (y : seen)
