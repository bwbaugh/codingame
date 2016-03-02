import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let laps = read input_line :: Int
    input_line <- getLine
    let checkpointcount = read input_line :: Int
    
    replicateM checkpointcount $ do
        input_line <- getLine
        let input = words input_line
        let checkpointx = read (input!!0) :: Int
        let checkpointy = read (input!!1) :: Int
        return ()
    loop

loop :: IO ()
loop = do
    
    replicateM 2 $ do
        input_line <- getLine
        let input = words input_line
        let x = read (input!!0) :: Int
        let y = read (input!!1) :: Int
        let vx = read (input!!2) :: Int
        let vy = read (input!!3) :: Int
        let angle = read (input!!4) :: Int
        let nextcheckpointid = read (input!!5) :: Int
        return ()
    
    replicateM 2 $ do
        input_line <- getLine
        let input = words input_line
        let x = read (input!!0) :: Int
        let y = read (input!!1) :: Int
        let vx = read (input!!2) :: Int
        let vy = read (input!!3) :: Int
        let angle = read (input!!4) :: Int
        let nextcheckpointid = read (input!!5) :: Int
        return ()
    
    -- hPutStrLn stderr "Debug messages..."
    
    -- Write action to stdout
    putStrLn "8000 4500 100"
    putStrLn "8000 4500 100"
    
    loop
