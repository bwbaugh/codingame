import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let input = words input_line
    let p = read (input!!0) :: Int -- number of players in the game (2 to 4 players)
    let id = read (input!!1) :: Int -- ID of your player (0, 1, 2, or 3)
    let d = read (input!!2) :: Int -- number of drones in each team (3 to 11)
    let z = read (input!!3) :: Int -- number of zones on the map (4 to 8)
    
    replicateM z $ do
        input_line <- getLine
        let input = words input_line
        let x = read (input!!0) :: Int -- corresponds to the position of the center of a zone. A zone is a circle with a radius of 100 units.
        let y = read (input!!1) :: Int
        return ()
    loop z p d

loop :: Int -> Int -> Int -> IO ()
loop z p d = do
    
    replicateM z $ do
        input_line <- getLine
        let tid = read input_line :: Int -- ID of the team controlling the zone (0, 1, 2, or 3) or -1 if it is not controlled. The zones are given in the same order as in the initialization.
        return ()
    
    replicateM p $ do
        
        replicateM d $ do
            input_line <- getLine
            let input = words input_line
            let dx = read (input!!0) :: Int -- The first D lines contain the coordinates of drones of a player with the ID 0, the following D lines those of the drones of player 1, and thus it continues until the last player.
            let dy = read (input!!1) :: Int
            return ()
        return ()
    
    replicateM d $ do
        
        -- hPutStrLn stderr "Debug messages..."
        
        -- output a destination point to be reached by one of your drones. The first line corresponds to the first of your drones that you were provided as input, the next to the second, etc.
        
        putStrLn "20 20"
        return ()
    
    loop z p d
