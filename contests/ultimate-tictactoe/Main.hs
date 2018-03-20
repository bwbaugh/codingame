import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    loop

loop :: IO ()
loop = do
    input_line <- getLine
    let input = words input_line
    let opponentrow = read (input!!0) :: Int
    let opponentcol = read (input!!1) :: Int
    input_line <- getLine
    let validactioncount = read input_line :: Int
    
    replicateM validactioncount $ do
        input_line <- getLine
        let input = words input_line
        let row = read (input!!0) :: Int
        let col = read (input!!1) :: Int
        return ()
    
    -- hPutStrLn stderr "Debug messages..."
    
    -- Write action to stdout
    putStrLn "0 0"
    
    loop
