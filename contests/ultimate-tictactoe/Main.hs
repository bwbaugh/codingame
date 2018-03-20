{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}
module Main (main) where

import           Control.Monad (forever, replicateM)
import           System.IO
    ( BufferMode (NoBuffering)
    , hSetBuffering
    , stdout
    )

type Point = (Int, Int)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    forever $ do
        [oppRow, oppCol] <- map read . words <$> getLine :: IO [Int]
        actions <- readLn >>= flip replicateM
            ((\[row, col] -> (row, col)) . map read . words <$> getLine)
        (putStrLn . showPoint) $ move (oppRow, oppCol) actions

showPoint :: Point -> String
showPoint (row, col) = (unwords . map show) [row, col]

move :: Point -> [Point] -> Point
move _ = head
