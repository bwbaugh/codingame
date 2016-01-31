import Control.Arrow (second)
import Data.List
import Data.Maybe
import System.IO

data Direction = SOUTH | EAST | NORTH | WEST
    deriving (Eq, Ord, Show, Bounded, Enum)

type Cell = Char
type Grid = [[Cell]]
type Position = (Int, Int)

data BenderState = BenderState {
    breakerMode :: Bool
    }
    deriving (Show)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    _ <- getLine
    grid <- fmap lines getContents
    -- TODO(bwbaugh|2016-01-31): Consider including direction check.
    let (points, directions) =
            unzip $
            genPath grid (initialPosition grid) initialDirection initialState
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

initialState :: BenderState
initialState = BenderState {
    breakerMode = False
    }

genPath ::
    Grid
    -> Position
    -> Direction
    -> BenderState
    -> [(Position, Direction)]
genPath grid position direction state
    | nextCell == '$' = [(position, direction)]
    -- TODO(bwbaugh|2016-01-30): Handle reverse order when inverted.
    | nextCell `elem` obstacles =
        genPath grid position (changeDirection grid position) state
    | nextCell == 'N' =
        (position, direction) : genPath grid position' NORTH state
    | nextCell == 'S' =
        (position, direction) : genPath grid position' SOUTH state
    | nextCell == 'E' =
        (position, direction) : genPath grid position' EAST state
    | nextCell == 'W' =
        (position, direction) : genPath grid position' WEST state
    -- TODO(bwbaugh|2016-01-30): Handle circuit inverters 'I',
    --   Breaker mode 'B', and teleporters 'T'.
    | otherwise =
        (position, direction) : genPath grid position' direction state
    where
    nextCell = getCell grid position'
    position' = nextPos direction position

obstacles :: [Cell]
obstacles = "X#"

nextPos :: Direction -> Position -> Position
-- TODO(bwbaugh|2016-01-30): Handle out of bounds (return a Maybe Position).
nextPos NORTH (row, column) = (row - 1, column)
nextPos SOUTH (row, column) = (row + 1, column)
nextPos EAST (row, column) = (row, column + 1)
nextPos WEST (row, column) = (row, column - 1)

getCell :: Grid -> Position -> Cell
getCell grid (row, column) = grid !! row !! column

changeDirection :: Grid -> Position -> Direction
changeDirection grid position = head $ filter isValid directions
    where
    directions :: [Direction]
    directions = [minBound..maxBound]
    isValid :: Direction -> Bool
    isValid direction = getCell grid position' `notElem` obstacles
        where
            position' = nextPos direction position

-- TODO(bwbaugh|2016-01-30): Speed up by using a set or similar.
containsDuplicate :: (Eq a) => [a] -> Bool
containsDuplicate xs = check xs []
    where
    check :: (Eq a) => [a] -> [a] -> Bool
    check [] _ = False
    check (y:ys) seen = y `elem` seen || check ys (y : seen)
