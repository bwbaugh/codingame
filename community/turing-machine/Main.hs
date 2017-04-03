{-# OPTIONS_GHC -Wall #-}
import           Control.Monad   (replicateM)
import qualified Data.Map.Strict as Map
import           System.IO
    ( BufferMode (NoBuffering)
    , hSetBuffering
    , stdout
    )

data TuringMachine = TuringMachine
    { tSymbols    :: !Int
    , tLength     :: !Int
    , tPosition   :: !Int
    -- TODO: Change to sequence.
    , tTape       :: [Symbol]
    , tState      :: State
    , tStateTable :: StateTable
    } deriving (Show)

type Symbol = Int

data State = HALT | State String deriving (Eq, Ord, Read, Show)

type StateTable = Map.Map (State, Symbol) Action

data Action = Action
    { aSymbol    :: Char
    , aDirection :: Direction
    , aNext      :: State
    } deriving (Show)

data Direction = L | R deriving (Eq, Show, Read)

main :: IO ()
main =
    hSetBuffering stdout NoBuffering >>
    readTuringMachine >>=
    uncurry displayState . runMachine 0

readTuringMachine :: IO TuringMachine
readTuringMachine = do
    [symbols, tapeLength, position] <- map read . words <$> getLine :: IO [Int]
    start <- State <$> getLine
    table <- readStateTable
    return TuringMachine
        { tSymbols = symbols
        , tLength = tapeLength
        , tPosition = position
        , tTape = replicate tapeLength 0
        , tState = start
        , tStateTable = table
        }

readStateTable :: IO StateTable
readStateTable =
    Map.fromList . concat <$>
    (readLn >>= flip replicateM (parseActions <$> getLine))

parseActions :: String -> [((State, Symbol), Action)]
parseActions = undefined

runMachine :: Int -> TuringMachine -> (Int, TuringMachine)
runMachine a m = (a, m)

displayState :: Int -> TuringMachine -> IO ()
displayState actions machine = do
    print actions
    print $ tPosition machine
    putStrLn . concatMap show $ tTape machine
