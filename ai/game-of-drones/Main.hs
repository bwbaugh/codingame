import Control.Monad
import System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    [    numPlayers -- ^ number of players in the game (2 to 4 players)
        , playerId -- ^ ID of your player (0, 1, 2, or 3)
        , numDrones -- ^ number of drones in each team (3 to 11)
        , numZones -- ^ number of zones on the map (4 to 8)
        ] <- fmap (map read . words) getLine :: IO [Int]
    zones <- replicateM numZones $
        -- corresponds to the position of the center of a zone. A zone
        -- is a circle with a radius of 100 units.
        fmap ((\[x, y] -> (x, y)) . map read . words) getLine :: IO [(Int, Int)]
    forever $ do
        -- ID of the team controlling the zone (0, 1, 2, or 3) or -1 if
        -- it is not controlled. The zones are given in the same order
        -- as in the initialization.
        owners <- replicateM numZones readLn :: IO [Int]
        drones <- replicateM numPlayers $
            replicateM numDrones $
                -- The first D lines contain the coordinates of drones
                -- of a player with the ID 0, the following D lines
                -- those of the drones of player 1, and thus it
                -- continues until the last player.
                fmap ((\[x, y] -> (x, y)) . map read . words) getLine
                :: IO [[(Int, Int)]]
        replicateM numDrones $
            -- output a destination point to be reached by one of your
            -- drones. The first line corresponds to the first of your
            -- drones that you were provided as input, the next to the
            -- second, etc.
            putStrLn "20 20"
