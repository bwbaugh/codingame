{-# OPTIONS_GHC -Wall #-}
module Main where

import           Control.Applicative ((<$>), (<*>))
import           Control.Arrow       (first, second)
import           Control.Monad       (replicateM)
import           Data.Function       (on)
import           Data.List           (partition, sortBy)
import           System.IO
    ( BufferMode (NoBuffering)
    , hSetBuffering
    , stdout
    )

import qualified Data.Map.Strict     as M

data Base = TopLeft | BotRight deriving (Eq, Show)

data InitialState = InitialState
    { bustersPerPlayer :: !Int  -- ^ The amount of busters you control.
    , ghostCount       :: !Int -- ^ The amount of ghosts on the map.
    , myBase           :: !Base -- ^ The base for the team you are on.
    } deriving (Show)

data Entity state team = Entity
    { eId    :: !Int  -- ^ Buster-ID or Ghost-ID.
    , ePos   :: !(Int, Int)  -- ^ An (x, y) point.
    , eState :: !state
    , eTeam  :: !team
    } deriving (Show)

newtype BusterState = CarryingGhost (Maybe GhostId) deriving (Show)

type GhostId = Int

newtype GhostState = BustersTrapping Int deriving (Show)

type Buster = Entity BusterState Base

type Ghost = Entity GhostState ()

-- | Wrapper to allow returning one of these types.
data AnEntity = ABuster Buster | AGhost Ghost deriving (Show)

data Move = MOVE Int Int | BUST Int | RELEASE deriving (Show)

type SearchTasks = M.Map BusterId (Int, Int)

type BusterId = Int

-- | Send your busters out into the fog to trap ghosts and bring them home!
main :: IO ()
main = hSetBuffering stdout NoBuffering >> readInitialState >>= loop M.empty

readInitialState :: IO InitialState
readInitialState = InitialState <$> readLn <*> readLn <*> (readBase <$> readLn)

readBase :: Int -> Base
readBase 0 = TopLeft
readBase 1 = BotRight
readBase x = error $ "Unknown team-ID: " ++ show x

loop :: SearchTasks -> InitialState -> IO ()
loop tasks initialState =
    move initialState tasks <$>
    (readLn >>= flip replicateM readEntity) >>= \(moves, tasks') ->
    mapM_ print moves >>
    loop tasks' initialState

readEntity :: IO AnEntity
readEntity = do
    [entityId, x, y, entityType', _, value] <- map read . words <$> getLine
    let parseBuster' base = ABuster $ parseBuster entityId (x, y) base value
        in case entityType' of
            0 -> return $ parseBuster' TopLeft
            1 -> return $ parseBuster' BotRight
            (-1) -> return $ AGhost (parseGhost entityId (x, y) value)
            t -> error $ "Unknown entity type: " ++ show t

parseBuster :: Int -> (Int, Int) -> Base -> Int -> Buster
parseBuster entityId pos base value = Entity
    { eId = entityId
    , ePos = pos
    , eState = CarryingGhost $ case value of
        (-1) -> Nothing
        x -> Just x
    , eTeam = base
    }

parseGhost :: Int -> (Int, Int) -> Int -> Ghost
parseGhost entityId pos value = Entity
    { eId = entityId
    , ePos = pos
    , eState = BustersTrapping value
    , eTeam = ()
    }

move :: InitialState -> SearchTasks -> [AnEntity] -> ([Move], SearchTasks)
move initialState tasks entities = (moves, tasks')
  where
    moves = orderMoves $
        map release            releasing ++
        map goHome             notreleasing ++
        map (second bust)      busting ++
        map (second goToGhost) moving ++
        map (second goto)      searching

    -- | Order moves by buster-ID as expected by the game.
    orderMoves :: [(Buster, Move)] -> [Move]
    orderMoves = map snd . sortBy (compare `on` fst) . map (first eId)

    busters = [x | ABuster x <- entities, eTeam x == myBase initialState]
    ghosts = [x | AGhost x <- entities]

    (carrying, notcarrying) = partition isCarrying busters
    (releasing, notreleasing) = partition (releaseByHome initialState) carrying
    (paired, unpaired) = pairGhosts notcarrying ghosts
    (busting, moving) = partition ((== EQ) . uncurry bustRange) paired

    release       b = (b, RELEASE)
    -- TODO: Go to closest point within releasing distance of base.
    goHome        b = (b, goto (baseLocation (myBase initialState)))
    moveToSearch  b = (b, nextSearch tasks b)

    searching = map moveToSearch unpaired
    tasks' = M.fromList (map (first eId) searching) `M.union` tasks

isCarrying :: Buster -> Bool
isCarrying Entity {eState = CarryingGhost Nothing} = False
isCarrying _                                       = True

-- | Check if within release distance of home base.
releaseByHome :: InitialState -> Buster -> Bool
releaseByHome InitialState {myBase = base} b =
    distance (ePos b) (baseLocation base) < 1600

-- | Create pairs of busters and ghosts, and any unpaired busters.
pairGhosts :: [Buster] -> [Ghost] -> ([(Buster, Ghost)], [Buster])
pairGhosts [] _ = ([], [])
pairGhosts bs [] = ([], bs)
pairGhosts (b:bs) gs = ((b, g) : paired, unpaired)
  where
    (paired, unpaired) = pairGhosts bs gs'
    (g:gs') = sortBy (compare `on` distance (ePos b) . ePos) gs

distance :: Integral a => (a, a) -> (a, a) -> Double
distance (x1, y1) (x2, y2) =
    sqrt $ (fromIntegral x2 - fromIntegral x1) ** 2 +
           (fromIntegral y2 - fromIntegral y1) ** 2

bustRange :: Buster -> Ghost -> Ordering
bustRange b g
    | d < 900   = LT
    | d > 1760  = GT
    | otherwise = EQ
  where
    d = distance (ePos b) (ePos g)

bust :: Ghost -> Move
bust = BUST . eId

-- TODO: Move a shorter distance by knowing where we are coming from.
goToGhost :: Ghost -> Move
goToGhost = goto . ePos

goToTheirBase :: InitialState -> Move
goToTheirBase = goto . baseLocation . theirBase

theirBase :: InitialState -> Base
theirBase InitialState {myBase = TopLeft} = BotRight
theirBase InitialState {myBase = BotRight} = TopLeft

baseLocation :: Base -> (Int, Int)
baseLocation TopLeft = (0, 0)
baseLocation BotRight = (16000, 9000)

goto :: (Int, Int) -> Move
goto (x, y) = MOVE x y

nextSearch :: SearchTasks -> Buster -> (Int, Int)
nextSearch tasks Entity {eId = bId, ePos = pos} =
    case M.lookup bId tasks of
        Nothing -> head searchPoints
        Just current -> if pos == current then target else current
  where
    target = head . tail . dropWhile (/= pos) . cycle $ searchPoints

-- | Buster visibility range.
vRange :: Int
vRange = 2200

searchPoints :: [(Int, Int)]
searchPoints =
    [ (2100,2100)
    , (2100,4300)
    , (2100,6900)
    , (4300,6900)
    , (4300,4300)
    , (4300,2100)
    , (6500,2200)
    , (6500,4300)
    , (6500,6900)
    , (8700,6900)
    , (8700,4300)
    , (8700,2100)
    , (10900,2100)
    , (10900,4300)
    , (10900,6900)
    , (13900,6900)
    , (13900,4300)
    , (13900,2100)
    ]
