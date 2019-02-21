{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}
module Main (main) where

import           Control.Monad (replicateM)
import           System.IO
    ( BufferMode (NoBuffering)
    , hSetBuffering
    , stdout
    )

type Point = (Int, Int)

data Cell
    = Empty
    | Us
    | Them
    deriving (Eq, Show)

type SmallBoard = [[Cell]]

type LargeBoard = [[SmallBoard]]

blankSmall :: SmallBoard
blankSmall =
    [ [Empty, Empty, Empty]
    , [Empty, Empty, Empty]
    , [Empty, Empty, Empty]
    ]

blankLarge :: LargeBoard
blankLarge =
    [ [blankSmall, blankSmall, blankSmall]
    , [blankSmall, blankSmall, blankSmall]
    , [blankSmall, blankSmall, blankSmall]
    ]

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    loop blankLarge

loop :: LargeBoard -> IO ()
loop oldBoard = do
    oppMove <- readMove
    actions <- readLn >>= flip replicateM
        (toPoint . map read . words <$> getLine)
    let oppBoard = makeMove Them oldBoard oppMove
        ourMove = move oppBoard oppMove actions
        newBoard = makeMove Us oppBoard ourMove
    (putStrLn . showPoint) ourMove
    loop newBoard

toPoint :: [Int] -> Point
toPoint [row, col] = (row, col)
toPoint x          = error (show x)

readMove :: IO Point
readMove = toPoint . map read . words <$> getLine

showPoint :: Point -> String
showPoint (row, col) = (unwords . map show) [row, col]

makeMove :: Cell -> LargeBoard -> Point -> LargeBoard
makeMove _ b (-1, -1) = b
makeMove c b (row, col) = go (r1, c1) small' b
  where
    small :: SmallBoard
    small = b !! r1 !! c1
    small' = go (r2, c2) c small
    go (r0, c0) x m = replaceAtIndex r0 (replaceAtIndex c0 x (m !! r0)) m
    (r1, r2) = row `divMod` 3
    (c1, c2) = col `divMod` 3

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item:b)
  where
    (a, _:b) = splitAt n ls

move :: LargeBoard -> Point -> [Point] -> Point
move _ _ = head
