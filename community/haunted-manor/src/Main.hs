module Main where

import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let input = words input_line
    let vampirecount = read (input!!0) :: Int
    let zombiecount = read (input!!1) :: Int
    let ghostcount = read (input!!2) :: Int
    input_line <- getLine
    let size = read input_line :: Int
    input_line <- getLine
    let input = words input_line
    
    forM [0..(size-1)] $ \i -> do
        let canseefromtop = read (input!!(i)) :: Int
        return ()
    input_line <- getLine
    let input = words input_line
    
    forM [0..(size-1)] $ \i -> do
        let canseefrombottom = read (input!!(i)) :: Int
        return ()
    input_line <- getLine
    let input = words input_line
    
    forM [0..(size-1)] $ \i -> do
        let canseefromleft = read (input!!(i)) :: Int
        return ()
    input_line <- getLine
    let input = words input_line
    
    forM [0..(size-1)] $ \i -> do
        let canseefromright = read (input!!(i)) :: Int
        return ()
    
    replicateM size $ do
        row <- getLine
        return ()
    
    -- hPutStrLn stderr "Debug messages..."
    
    -- Write answer to stdout
    putStrLn "answer"
    return ()
