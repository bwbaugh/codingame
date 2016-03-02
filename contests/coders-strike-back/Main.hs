import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    laps <- readLn :: IO Int
    checkpointcount <- readLn :: IO Int
    checkpoints <- replicateM checkpointcount $
        fmap (map read . words) getLine :: IO [[Int]]

    forever $ do
        myPods <- replicateM 2 readPod
        enemyPods <- replicateM 2 readPod

        putStrLn "8000 4500 100"
        putStrLn "8000 4500 100"

-- | [x, y, vx, vy, angle, nextcheckpointid]
readPod :: IO [Int]
readPod = fmap (map read . words) getLine
