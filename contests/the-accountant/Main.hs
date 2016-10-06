import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Shoot enemies before they collect all the incriminating data!
    -- The closer you are to an enemy, the more damage you do but don't get too close or you'll get killed.
    
    loop

loop :: IO ()
loop = do
    input_line <- getLine
    let input = words input_line
    let x = read (input!!0) :: Int
    let y = read (input!!1) :: Int
    input_line <- getLine
    let datacount = read input_line :: Int
    
    replicateM datacount $ do
        input_line <- getLine
        let input = words input_line
        let dataid = read (input!!0) :: Int
        let datax = read (input!!1) :: Int
        let datay = read (input!!2) :: Int
        return ()
    input_line <- getLine
    let enemycount = read input_line :: Int
    
    replicateM enemycount $ do
        input_line <- getLine
        let input = words input_line
        let enemyid = read (input!!0) :: Int
        let enemyx = read (input!!1) :: Int
        let enemyy = read (input!!2) :: Int
        let enemylife = read (input!!3) :: Int
        return ()
    
    -- hPutStrLn stderr "Debug messages..."
    
    -- MOVE x y or SHOOT id
    putStrLn "MOVE 8000 4500"
    
    loop
