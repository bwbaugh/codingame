import System.IO
import Control.Monad

data Pod = Pod {
      getPos :: Position
    , getSpeed :: Position
    , getAngle :: Int
    , getCheckPoint :: Int
    } deriving (Show)

type Checkpoint = Position
type Position = (Int, Int)
type Thrust = Int

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    laps <- readLn :: IO Int
    checkpointcount <- readLn :: IO Int
    checkpoints <- replicateM checkpointcount $ do
        [x, y] <- fmap (map read . words) getLine
        return (x, y)

    forever $ do
        myPods <- replicateM 2 readPod
        enemyPods <- replicateM 2 readPod
        forM_ myPods $ \pod -> do
            let ((x, y), thrust) = pickMove checkpoints pod
            putStrLn . unwords . map show $ [x, y, thrust]

readPod :: IO Pod
readPod = do
    [x, y, vx, vy, angle, nextCheckPointId] <- fmap (map read . words) getLine
    return $ Pod (x, y) (vx, vy) angle nextCheckPointId

pickMove :: [Checkpoint] -> Pod -> (Position, Thrust)
pickMove checkpoints pod = (position, thrust)
  where
    position = checkpoints !! getCheckPoint pod
    thrust = 100
