import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Send your busters out into the fog to trap ghosts and bring them home!
    
    input_line <- getLine
    let bustersperplayer = read input_line :: Int -- the amount of busters you control
    input_line <- getLine
    let ghostcount = read input_line :: Int -- the amount of ghosts on the map
    input_line <- getLine
    let myteamid = read input_line :: Int -- if this is 0, your base is on the top left of the map, if it is one, on the bottom right
    loop bustersperplayer

loop :: Int -> IO ()
loop bustersperplayer = do
    input_line <- getLine
    let entities = read input_line :: Int -- the number of busters and ghosts visible to you
    
    replicateM entities $ do
        input_line <- getLine
        let input = words input_line
        let entityid = read (input!!0) :: Int -- buster id or ghost id
        let x = read (input!!1) :: Int
        let y = read (input!!2) :: Int -- position of this buster / ghost
        let entitytype = read (input!!4) :: Int -- the team id if it is a buster, -1 if it is a ghost.
        let state = read (input!!4) :: Int -- For busters: 0=idle, 1=carrying a ghost.
        let value = read (input!!5) :: Int -- For busters: Ghost id being carried. For ghosts: number of busters attempting to trap this ghost.
        return ()
    
    replicateM bustersperplayer $ do
        
        -- hPutStrLn stderr "Debug messages..."
        
        -- MOVE x y | BUST id | RELEASE
        putStrLn ("MOVE 8000 4500")
        return ()
    
    loop bustersperplayer
