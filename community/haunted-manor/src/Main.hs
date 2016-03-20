module Main where

import Control.Monad
import Data.Foldable (toList)
import Data.Maybe
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!))

type Manor = V.Vector Row
type Row = V.Vector Cell
type Cell = Int
(empty:diagonalup:diagonaldown:vampire:zombie:ghost:_) = [1..]
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
data Gaze = Direct | Mirror deriving (Eq)

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
parseManor = V.fromList . map (V.fromList . map parseCell)

parseCell :: Char -> Cell
parseCell '.' = empty
parseCell '\\' = diagonaldown
parseCell '/' = diagonalup
parseCell 'V' = vampire
parseCell 'Z' = zombie
parseCell 'G' = ghost
parseCell x = error $ "unexpected input grid char: " ++ [x]

showManor :: Manor -> String
showManor = unlines . map (concatMap showCell) . toList

showCell :: Cell -> String
showCell cell
    | cell == empty = "."
    | cell == diagonaldown = "\\"
    | cell == diagonalup = "/"
    | cell == vampire = "V"
    | cell == zombie = "Z"
    | cell == ghost = "G"

validSolutions :: Manor -> Count -> Seen -> [Manor]
validSolutions m c s = filter (`checkSeen` s) $ possibleSolutions m c s

possibleSolutions :: Manor -> Count -> Seen -> [Manor]
possibleSolutions manor count seen =
    map (\(m, _, _) -> m) $ foldM (genRow seen) (V.empty, count, -1) manor

genRow :: Seen -> (Manor, Count, Int) -> Row -> [(Manor, Count, Int)]
genRow seen (acc, count, idx) row = do
    (row', count') <- foldM genCell (V.empty, count) row
    let idx' = idx + 1
        acc' = acc `V.snoc` row'
        left = genSeen acc' East
        right = genSeen acc' West
        top = genSeen acc' South
    guard $ (left !! idx') <= seenLeft seen !! idx'
    guard $ (right !! idx') <= seenRight seen !! idx'
    guard $ top <= seenTop seen
    return (acc', count', idx')

genCell :: (Row, Count) -> Cell -> [(Row, Count)]
genCell (acc, count) cell = do
    cell' <- if cell == empty then availableMonsters count else [cell]
    return (acc `V.snoc` cell', subtractCell count cell')

subtractCell :: Count -> Cell -> Count
subtractCell count@(Count v z g) cell
    | cell == vampire = Count (v - 1) z g
    | cell == zombie = Count v (z - 1) g
    | cell == ghost = Count v z (g - 1)
    | otherwise = count

availableMonsters :: Count -> [Cell]
availableMonsters (Count v z g) = catMaybes [v', z', g']
  where
    v' = if v > 0 then Just vampire else Nothing
    z' = if z > 0 then Just zombie else Nothing
    g' = if g > 0 then Just ghost else Nothing

checkSeen :: Manor -> Seen -> Bool
checkSeen = (==) . visibleMonsters

visibleMonsters :: Manor -> Seen
visibleMonsters manor = Seen (go South) (go North) (go East) (go West)
  where
    go = genSeen manor

genSeen :: Manor -> Direction -> [Int]
genSeen m d = map (visible . look m d) [0..V.length m - 1]

look :: Manor -> Direction -> Int -> [Cell]
look manor South col =  path manor South 0 col
look manor North col = path manor North (V.length manor - 1) col
look manor East row = path manor East row 0
look manor West row = path manor West row (V.length (manor ! 0) - 1)

path :: Manor -> Direction -> Int -> Int -> [Cell]
path manor direction row col
    | row < 0 || row > V.length manor - 1 = []
    | col < 0 || col > V.length (manor ! 0) - 1 = []
    | otherwise = cell : path manor direction' row' col'
  where
    cell = (manor ! row) ! col
    direction' = newDirection direction cell
    (row', col') = move direction' row col

newDirection :: Direction -> Cell -> Direction
newDirection South cell
    | cell == diagonaldown = East
    | cell == diagonalup = West
    | otherwise = South
newDirection North cell
    | cell == diagonaldown = West
    | cell == diagonalup = East
    | otherwise = North
newDirection East cell
    | cell == diagonaldown = South
    | cell == diagonalup = North
    | otherwise = East
newDirection West cell
    | cell == diagonaldown = North
    | cell == diagonalup = South
    | otherwise = West

move :: Direction -> Int -> Int -> (Int, Int)
move South row col = (row + 1, col)
move North row col = (row - 1, col)
move East row col = (row, col + 1)
move West row col = (row, col - 1)

visible :: [Cell] -> Int
visible = go Direct
  where
    go _ [] = 0
    go gaze (c:xs)
        | c == empty = go gaze xs
        | c == diagonaldown = go Mirror xs
        | c == diagonalup = go Mirror xs
        | c == vampire && gaze == Direct = 1 + go Direct xs
        | c == vampire && gaze == Mirror = go Mirror xs
        | c == zombie = 1 + go gaze xs
        | c == ghost && gaze == Direct = go Direct xs
        | c == ghost && gaze == Mirror = 1 + go Mirror xs
        | otherwise = error "visible non-exhaustive"
