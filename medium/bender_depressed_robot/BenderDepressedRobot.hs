import Control.Arrow (second)
import Control.Monad
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
    , invertDirections :: Bool
    }
    deriving (Eq, Show)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    _ <- getLine
    grid <- fmap lines getContents
    let path =
            genPath grid (initialPosition grid) initialDirection initialState
    if containsDuplicate path
        then putStrLn "LOOP"
        else forM_ path $ \(position, direction, state, grid) -> do
            hPutStr stderr $ unlines $ replaceCell grid position 'x'
            hPrint stderr position
            hPrint stderr state
            print direction

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
    , invertDirections = False
    }

genPath ::
    Grid
    -> Position
    -> Direction
    -> BenderState
    -> [(Position, Direction, BenderState, Grid)]
genPath grid position direction state
    | breakerMode state && nextCell == 'X' =
        genPath (removeObstacle grid position') position direction state
    | nextCell `elem` obstacles =
        genPath grid position
            (changeDirection grid position (invertDirections state)) state
    | nextCell == '$' = [x]
    | nextCell == 'B' =
        x : genPath grid position' direction
            (state { breakerMode = not (breakerMode state)})
    | nextCell == 'I' =
        x : genPath grid position' direction
            (state { invertDirections = not (invertDirections state)})
    | nextCell == 'T' = x : genPath grid teleportPosition direction state
    | nextCell == 'N' = x : genPath grid position' NORTH state
    | nextCell == 'S' = x : genPath grid position' SOUTH state
    | nextCell == 'E' = x : genPath grid position' EAST state
    | nextCell == 'W' = x : genPath grid position' WEST state
    | otherwise = x : genPath grid position' direction state
    where
    x = (position, direction, state, grid)
    nextCell = getCell grid position'
    position' = nextPos direction position
    teleportPosition = teleport grid position'

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

removeObstacle :: Grid -> Position -> Grid
removeObstacle grid (row, column) = replaceCell grid (row, column) ' '

replaceCell :: Grid -> Position -> Cell -> Grid
replaceCell grid (row, column) cell = replaceAtIndex row row' grid
    where
    row' = replaceAtIndex column cell (grid !! row)

-- http://stackoverflow.com/a/10133429/1988505
replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item:b) where (a, _:b) = splitAt n ls

changeDirection :: Grid -> Position -> Bool -> Direction
changeDirection grid position isReverse = head $ filter isValid directions
    where
    directions :: [Direction]
    directions = if isReverse
        then reverse [minBound..maxBound]
        else [minBound..maxBound]
    isValid :: Direction -> Bool
    isValid direction = getCell grid position' `notElem` obstacles
        where
            position' = nextPos direction position

teleport :: Grid -> Position -> Position
teleport grid position = otherTeleporter
    where
    otherTeleporter = head $ filter (/= position) locations
    locations =
        concatMap (\(x, ys) -> [(x, y) | y <- ys]) $
        filter ((not . null) . snd) $ zip [0..] (map (elemIndices 'T') grid)

-- TODO(bwbaugh|2016-01-30): Speed up by using a set or similar.
containsDuplicate :: (Eq a) => [a] -> Bool
containsDuplicate xs = check xs []
    where
    check :: (Eq a) => [a] -> [a] -> Bool
    check [] _ = False
    check (y:ys) seen = y `elem` seen || check ys (y : seen)
