import System.IO
import Control.Monad

data Pod = Pod {
      getPos :: (Int, Int)
    , getSpeed :: (Int, Int)
    , getAngle :: Int
    , getCheckPoint :: Int
    } deriving (Show)

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
        forM_ myPods $ \pod -> do
            let (x, y, thrust) = pickMove checkpoints pod
            putStrLn . unwords . map show $ [x, y, thrust]

readPod :: IO Pod
readPod = do
    [x, y, vx, vy, angle, nextCheckPointId] <- fmap (map read . words) getLine
    return $ Pod (x, y) (vx, vy) angle nextCheckPointId

pickMove :: [[Int]] -> Pod -> (Int, Int, Int)
pickMove checkpoints pod = (x, y, thrust)
  where
    [x, y] = checkpoints !! getCheckPoint pod
    thrust = 100
