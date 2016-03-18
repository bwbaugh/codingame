module Main where

import Control.Monad

type Manor = [[Cell]]
data Cell =
      Empty
    | DiagonalUp
    | DiagonalDown
    | Vampire
    | Zombie
    | Ghost
    deriving (Eq, Show)
data Count = Count {
      numVampire :: Int
    , numZombie :: Int
    , numGhost :: Int
    } deriving (Eq, Show)
data Seen = Seen {
      seenTop :: [Int]
    , seenBot :: [Int]
    , seenLeft :: [Int]
    , seenRight :: [Int]
    } deriving (Eq, Show)
data Direction = South | North | East | West deriving (Show)
data Gaze = Direct | Mirror

main :: IO ()
main = do
    [vampire, zombie, ghost] <- readWSV :: IO [Int]
    let count = Count vampire zombie ghost
    size <- readLn
    [top, bot, left, right] <- replicateM 4 readWSV :: IO [[Int]]
    let seen = Seen top bot left right
    manor <- readManor size
    case validSolutions manor count seen of
        [] -> putStrLn "NONE"
        (x:_) -> putStr $ showManor x
  where
    -- | Whitespace Separated Values.
    readWSV :: Read a => IO [a]
    readWSV = fmap (map read . words) getLine

readManor :: Int -> IO Manor
readManor size = liftM parseManor (replicateM size getLine)

parseManor :: [String] -> Manor
parseManor = map (map parseCell)

parseCell :: Char -> Cell
parseCell '.' = Empty
parseCell '\\' = DiagonalDown
parseCell '/' = DiagonalUp
parseCell 'V' = Vampire
parseCell 'Z' = Zombie
parseCell 'G' = Ghost
parseCell x = error $ "unexpected input grid char: " ++ [x]

showManor :: Manor -> String
showManor = unlines . map (concatMap showCell)

showCell :: Cell -> String
showCell Empty = "."
showCell DiagonalDown = "\\"
showCell DiagonalUp = "/"
showCell Vampire = "V"
showCell Zombie = "Z"
showCell Ghost = "G"

validSolutions :: Manor -> Count -> Seen -> [Manor]
validSolutions m c s = filter (validManor c s) $ possibleSolutions m

possibleSolutions :: Manor -> [Manor]
possibleSolutions manor =
    forM manor $ \row ->
        forM row $ \cell ->
            case cell of
                Empty -> allMonsters
                x -> [x]

allMonsters :: [Cell]
allMonsters = [Vampire, Zombie, Ghost]

validManor :: Count -> Seen -> Manor -> Bool
validManor count seen manor = checkCount manor count && checkSeen manor seen

checkCount :: Manor -> Count -> Bool
checkCount = (==) . countMonsters

countMonsters :: Manor -> Count
countMonsters m = Count (count Vampire) (count Zombie) (count Ghost)
  where
    count x = length . filter (== x) $ concat m

checkSeen :: Manor -> Seen -> Bool
checkSeen = (==) . visibleMonsters

visibleMonsters :: Manor -> Seen
visibleMonsters manor = Seen (go South) (go North) (go East) (go West)
  where
    go direction = map (visible . look manor direction) [0..length manor - 1]

look :: Manor -> Direction -> Int -> [Cell]
look manor South col =  path manor South 0 col
look manor North col = path manor North (length manor - 1) col
look manor East row = path manor East row 0
look manor West row = path manor West row (length manor - 1)

path :: Manor -> Direction -> Int -> Int -> [Cell]
path manor direction row col
    | row < 0 || row > size = []
    | col < 0 || col > size = []
    | otherwise = cell : path manor direction' row' col'
  where
    size = length manor - 1
    cell = manor !! row !! col
    direction' = newDirection direction cell
    (row', col') = move direction' row col

newDirection :: Direction -> Cell -> Direction
newDirection South DiagonalDown = East
newDirection South DiagonalUp = West
newDirection North DiagonalDown = West
newDirection North DiagonalUp = East
newDirection East DiagonalDown = South
newDirection East DiagonalUp = North
newDirection West DiagonalDown = North
newDirection West DiagonalUp = South
newDirection direction _ = direction

move :: Direction -> Int -> Int -> (Int, Int)
move South row col = (row + 1, col)
move North row col = (row - 1, col)
move East row col = (row, col + 1)
move West row col = (row, col - 1)

visible :: [Cell] -> Int
visible = go Direct
  where
    go _ [] = 0
    go gaze (Empty:xs) = go gaze xs
    go _ (DiagonalDown:xs) = go Mirror xs
    go _ (DiagonalUp:xs) = go Mirror xs
    go Direct (Vampire:xs) = 1 + go Direct xs
    go Mirror (Vampire:xs) = go Mirror xs
    go gaze (Zombie:xs) = 1 + go gaze xs
    go Direct (Ghost:xs) = go Direct xs
    go Mirror (Ghost:xs) = 1 + go Mirror xs
