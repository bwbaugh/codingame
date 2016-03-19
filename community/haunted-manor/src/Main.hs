module Main where

import Control.Monad
import Data.Maybe

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
validSolutions m c s = filter (`checkSeen` s) $ possibleSolutions m c s

possibleSolutions :: Manor -> Count -> Seen -> [Manor]
possibleSolutions manor count seen =
    map (\(m, _, _) -> m) $ foldM (genRow seen) ([], count, -1) manor

genRow :: Seen -> (Manor, Count, Int) -> [Cell] -> [(Manor, Count, Int)]
genRow seen (acc, count, idx) row = do
    (row', count') <- foldM genCell ([], count) row
    let idx' = idx + 1
        acc' = acc ++ [row']
        left = genSeen acc' East
        right = genSeen acc' West
        top = genSeen acc' South
    guard $ (left !! idx') <= seenLeft seen !! idx'
    guard $ (right !! idx') <= seenRight seen !! idx'
    guard $ top <= seenTop seen
    return (acc', count', idx')

genCell :: ([Cell], Count) -> Cell -> [([Cell], Count)]
genCell (acc, count) cell = do
    cell' <- case cell of
        Empty -> availableMonsters count
        x -> [x]
    return (acc ++ [cell'], subtractCell count cell')

subtractCell :: Count -> Cell -> Count
subtractCell (Count v z g) Vampire = Count (v - 1) z g
subtractCell (Count v z g) Zombie = Count v (z - 1) g
subtractCell (Count v z g) Ghost = Count v z (g - 1)
subtractCell c _ = c

availableMonsters :: Count -> [Cell]
availableMonsters (Count v z g) = catMaybes [v', z', g']
  where
    v' = if v > 0 then Just Vampire else Nothing
    z' = if z > 0 then Just Zombie else Nothing
    g' = if g > 0 then Just Ghost else Nothing

checkSeen :: Manor -> Seen -> Bool
checkSeen = (==) . visibleMonsters

visibleMonsters :: Manor -> Seen
visibleMonsters manor = Seen (go South) (go North) (go East) (go West)
  where
    go = genSeen manor

genSeen :: Manor -> Direction -> [Int]
genSeen m d = map (visible . look m d) [0..length m - 1]

look :: Manor -> Direction -> Int -> [Cell]
look manor South col =  path manor South 0 col
look manor North col = path manor North (length manor - 1) col
look manor East row = path manor East row 0
look manor West row = path manor West row (length (head manor) - 1)

path :: Manor -> Direction -> Int -> Int -> [Cell]
path manor direction row col
    | row < 0 || row > length manor - 1 = []
    | col < 0 || col > length (head manor) - 1 = []
    | otherwise = cell : path manor direction' row' col'
  where
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
