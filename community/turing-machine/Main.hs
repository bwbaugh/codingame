import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let input = words input_line
    let s = read (input!!0) :: Int
    let t = read (input!!1) :: Int
    let x = read (input!!2) :: Int
    start <- getLine
    input_line <- getLine
    let n = read input_line :: Int
    
    replicateM n $ do
        stateactions <- getLine
        return ()
    return ()
