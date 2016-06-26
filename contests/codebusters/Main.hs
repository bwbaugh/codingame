{-# OPTIONS_GHC -Wall #-}
module Main where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad       (replicateM, replicateM_)
import           System.IO
    ( BufferMode (NoBuffering)
    , hSetBuffering
    , stdout
    )

data Base = TopLeft | BotRight deriving (Show)

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

-- | Send your busters out into the fog to trap ghosts and bring them home!
main :: IO ()
main = hSetBuffering stdout NoBuffering >> readInitialState >>= loop

readInitialState :: IO InitialState
readInitialState = InitialState <$> readLn <*> readLn <*> (readBase <$> readLn)

readBase :: Int -> Base
readBase 0 = TopLeft
readBase 1 = BotRight
readBase x = error $ "Unknown team-ID: " ++ show x

loop :: InitialState -> IO ()
loop initialState = do
    _ <- readLn >>= flip replicateM readEntity
    replicateM_ (bustersPerPlayer initialState) $
        print move
    loop initialState

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

move :: Move
move = MOVE 8000 4500
