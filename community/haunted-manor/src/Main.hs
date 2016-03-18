module Main where

import Control.Monad
import Data.Either
import Data.Maybe

type Manor = [[Maybe Cell]]
type Cell = Either Mirror Monster
data Mirror = DiagonalDown | DiagonalUp deriving (Show)
data Monster = Vampire | Zombie | Ghost deriving (Eq, Show)
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

main :: IO ()
main = do
    [vampire, zombie, ghost] <- readWSV :: IO [Int]
    let count = Count vampire zombie ghost
    size <- readLn
    [top, bot, left, right] <- replicateM 4 readWSV :: IO [[Int]]
    let seen = Seen top bot left right
    manor <- liftM parseManor (replicateM size getLine)
    case validSolutions manor count seen of
        [] -> putStrLn "NONE"
        (x:_) -> putStr $ showManor x
  where
    -- | Whitespace Separated Values.
    readWSV :: Read a => IO [a]
    readWSV = fmap (map read . words) getLine

parseManor :: [String] -> Manor
parseManor = map (map parseCell)

parseCell :: Char -> Maybe Cell
parseCell '.' = Nothing
parseCell '\\' = Just (Left DiagonalDown)
parseCell '/' = Just (Left DiagonalUp)
parseCell x = error $ "unexpected input grid char: " ++ [x]

showManor :: Manor -> String
showManor = unlines . map (concatMap (maybe "." showCell))

showCell :: Cell -> String
showCell (Left DiagonalDown) = "\\"
showCell (Left DiagonalUp) = "/"
showCell (Right Vampire) = "V"
showCell (Right Zombie) = "Z"
showCell (Right Ghost) = "G"

validSolutions :: Manor -> Count -> Seen -> [Manor]
validSolutions m c s = filter (validManor c s) $ possibleSolutions m

possibleSolutions :: Manor -> [Manor]
possibleSolutions manor =
    forM manor $ \row ->
        forM row $ \cell ->
            case cell of
                Nothing -> map (Just . Right) allMonsters
                x -> [x]

allMonsters :: [Monster]
allMonsters = [Vampire, Zombie, Ghost]

validManor :: Count -> Seen -> Manor -> Bool
validManor count seen manor = checkCount manor count && checkSeen manor seen

checkCount :: Manor -> Count -> Bool
checkCount = (==) . countMonsters

countMonsters :: Manor -> Count
countMonsters m = Count (count Vampire) (count Zombie) (count Ghost)
  where
    count :: Monster -> Int
    count x = length . filter (== x) $ concatMap (rights . catMaybes) m

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
    cell = fromMaybe (error "path encountered empty cell") $ manor !! row !! col
    direction' = newDirection direction cell
    (row', col') = move direction' row col

newDirection :: Direction -> Cell -> Direction
newDirection South (Left DiagonalDown) = East
newDirection South (Left DiagonalUp) = West
newDirection North (Left DiagonalDown) = West
newDirection North (Left DiagonalUp) = East
newDirection East (Left DiagonalDown) = South
newDirection East (Left DiagonalUp) = North
newDirection West (Left DiagonalDown) = North
newDirection West (Left DiagonalUp) = South
newDirection direction _ = direction

move :: Direction -> Int -> Int -> (Int, Int)
move South row col = (row + 1, col)
move North row col = (row - 1, col)
move East row col = (row, col + 1)
move West row col = (row, col - 1)

visible :: [Cell] -> Int
visible = undefined
