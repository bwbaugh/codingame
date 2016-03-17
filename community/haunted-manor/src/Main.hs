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

main :: IO ()
main = do
    [vampire, zombie, ghost] <- readWSV :: IO [Int]
    let count = Count vampire zombie ghost
    size <- readLn
    [top, bot, left, right] <- replicateM 4 readWSV :: IO [[Int]]
    let seen = Seen top bot left right
    manor <- liftM parseManor (replicateM size getLine)
    case possibleSolutions manor count seen of
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

possibleSolutions :: Manor -> Count -> Seen -> [Manor]
possibleSolutions manor count seen = do
    m <- forM manor $ \row ->
        forM row $ \cell ->
            case cell of
                Nothing -> map (Just . Right) allMonsters
                x -> [x]
    guard $ validManor m count seen
    return m

allMonsters :: [Monster]
allMonsters = [Vampire, Zombie, Ghost]

validManor :: Manor -> Count -> Seen -> Bool
validManor manor count seen = checkCount manor count && checkSeen manor seen

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
visibleMonsters = undefined
