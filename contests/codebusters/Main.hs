{-# OPTIONS_GHC -Wall #-}
module Main where

import           Control.Applicative ((<$>), (<*>))
import           Control.Arrow       (first, second)
import           Control.Monad       (replicateM)
import           Data.Function       (on)
import           Data.List           (delete, partition, sortBy)
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

instance Eq (Entity s t) where
    (==) = (==) `on` eId

newtype BusterState = CarryingGhost (Maybe GhostId) deriving (Show)

type GhostId = Int

newtype GhostState = BustersTrapping Int deriving (Show)

type Buster = Entity BusterState Base

type Ghost = Entity GhostState ()

-- | Wrapper to allow returning one of these types.
data AnEntity = ABuster Buster | AGhost Ghost deriving (Show)

data Move =
      MOVE Int Int
    | BUST GhostId
    | RELEASE
    | STUN BusterId
    deriving (Show)

type SearchTasks = M.Map BusterId (Int, Int)

type BusterId = Int

type LastStun = M.Map BusterId Turn

type Turn = Int

-- | Send your busters out into the fog to trap ghosts and bring them home!
main :: IO ()
main =
    hSetBuffering stdout NoBuffering >>
    readInitialState >>=
    loop M.empty M.empty 0

readInitialState :: IO InitialState
readInitialState = InitialState <$> readLn <*> readLn <*> (readBase <$> readLn)

readBase :: Int -> Base
readBase 0 = TopLeft
readBase 1 = BotRight
readBase x = error $ "Unknown team-ID: " ++ show x

loop :: SearchTasks -> LastStun -> Turn -> InitialState -> IO ()
loop tasks stun turn initialState =
    move initialState tasks stun turn <$>
    (readLn >>= flip replicateM readEntity) >>= \(moves, tasks', stun') ->
    mapM_ print moves >>
    loop tasks' stun' (turn + 1) initialState

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

move :: InitialState
     -> SearchTasks
     -> LastStun
     -> Turn
     -> [AnEntity]
     -> ([Move], SearchTasks, LastStun)
move initialState tasks stun turn entities = (moves, tasks', stun')
  where
    moves = orderMoves $
        map release               releasing ++
        map goHome                notreleasing ++
        map (second bust)         busting ++
        map (second goToGhost)    moving ++
        map (second (STUN . eId)) stunning ++
        map (second goto)         searching

    -- | Order moves by buster-ID as expected by the game.
    orderMoves :: [(Buster, Move)] -> [Move]
    orderMoves = map snd . sortBy (compare `on` fst) . map (first eId)

    busters = [x | ABuster x <- entities, eTeam x == myBase initialState]
    enemies = [x | ABuster x <- entities, eTeam x /= myBase initialState]
    ghosts = [x | AGhost x <- entities]

    (carrying, notcarrying) = partition isCarrying busters
    (releasing, notreleasing) = partition (releaseByHome initialState) carrying
    (paired, unpaired) = pairGhosts notcarrying ghosts
    (busting, moving) = partition ((== EQ) . uncurry bustRange) paired

    release       b = (b, RELEASE)
    -- TODO: Go to closest point within releasing distance of base.
    goHome        b = (b, goto (baseLocation (myBase initialState)))
    moveToSearch  b = (b, nextSearch (bustersPerPlayer initialState) tasks b)

    (stunning, notstunning) = pairStun stun turn unpaired enemies

    searching = map moveToSearch notstunning
    tasks' = M.fromList (map (first eId) searching) `M.union` tasks
    stun' =
        M.fromList (
            map (first eId . second (const turn)) stunning
        ) `M.union` stun

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
pairGhosts bs (g:gs) = ((b, g) : paired, unpaired)
  where
    (paired, unpaired) = pairGhosts bs' gs
    (b:bs') = sortBy (compare `on` distance (ePos g) . ePos) bs

pairStun :: LastStun
         -> Turn
         -> [Buster]
         -> [Buster]
         -> ([(Buster, Buster)], [Buster])
pairStun _    _    [] _      = ([], [])
pairStun _    _    bs []     = ([], bs)
pairStun stun turn bs (g:gs)
    | not (isCarrying g) = pairStun stun turn bs gs
    | otherwise =
        case bs' of
            (b:_) -> ((b, g) : paired, unpaired)
            []    -> pairStun stun turn bs gs
  where
    (paired, unpaired) = pairStun stun turn (delete (head bs') bs) gs
    bs' = filter stunUp  . filter withinDistance $ bs
    withinDistance = (<= 1760) . distance (ePos g) . ePos
    stunUp = maybe True ((turn >) . (20 +)) . (`M.lookup` stun) . eId

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

nextSearch :: Int -> SearchTasks -> Buster -> (Int, Int)
nextSearch totalBusters tasks b@Entity {eId = bId, ePos = pos} =
    case M.lookup bId tasks of
        Nothing -> head (searchPoints totalBusters b)
        Just current -> if pos == current then target else current
  where
    target =
        head . tail . dropWhile (/= pos) . cycle $ searchPoints totalBusters b

-- | Buster visibility range.
vRange :: Int
vRange = 2200

searchPoints :: Int -> Buster -> [(Int, Int)]
searchPoints totalBusters Entity {eId = bId', eTeam = team} =
    map snd . filter ((== bId) . (`rem` totalBusters) . fst) . zip [0..] . r $
        [ (2000,2000)
        , (2000,4300)
        , (2000,7000)
        , (4300,7000)
        , (4300,4300)
        , (4300,2000)
        , (6500,2200)
        , (6500,4300)
        , (6500,7000)
        , (8700,7000)
        , (8700,4300)
        , (8700,2000)
        , (10900,2000)
        , (10900,4300)
        , (10900,7000)
        , (13900,7000)
        , (13900,4300)
        , (13900,2000)
        ]
  where
    (r, bId) = case team of
        TopLeft  -> (id,      bId')
        BotRight -> (reverse, bId' - totalBusters)
