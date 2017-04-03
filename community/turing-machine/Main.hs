{-# OPTIONS_GHC -Wall #-}
import           Control.Monad   (replicateM)
import           Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import           System.IO
    ( BufferMode (NoBuffering)
    , hSetBuffering
    , stdout
    )

data TuringMachine = TuringMachine
    { tSymbols    :: !Int
    , tLength     :: !Int
    , tPosition   :: !Position
    , tTape       :: Tape
    , tState      :: State
    , tStateTable :: StateTable
    } deriving (Show)

type Position = Int

-- TODO: Change to sequence.
type Tape = [Symbol]

type Symbol = Int

data State = HALT | State String deriving (Eq, Ord, Read, Show)

type StateTable = Map.Map (State, Symbol) Action

data Action = Action
    { aSymbol    :: Int
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
parseActions xs = [((state, i), a) | (i, a) <- zip [0..] actions]
  where
    (state, actions) =
        (\(s, as) -> (readState s, readActions (tail as))) $ break (== ':') xs

readActions :: String -> [Action]
readActions = map readAction . splitOn ";"

readAction :: String -> Action
readAction xs = let [symbol, direction, next] = words xs in Action
    { aSymbol    = read symbol
    , aDirection = read direction
    , aNext      = readState next
    }

readState :: String -> State
readState "HALT" = HALT
readState xs     = State xs

runMachine :: Int -> TuringMachine -> (Int, TuringMachine)
runMachine a m = if isHalt m' then (a + 1, m') else runMachine (a + 1) m'
  where
    m' = step m

isHalt :: TuringMachine -> Bool
isHalt TuringMachine {tState = HALT} = True
isHalt TuringMachine {tPosition = p, tLength = l}
    | p < 0 = True
    | p >= l = True
    | otherwise = False

step :: TuringMachine -> TuringMachine
step machine = machine
    { tPosition = move direction (tPosition machine)
    , tTape = updateTape (tPosition machine) symbol (tTape machine)
    , tState = next
    }
  where
    Action {aSymbol = symbol, aDirection = direction, aNext = next} =
        Map.findWithDefault
            (error "unknown state") (tableKey machine) (tStateTable machine)

tableKey :: TuringMachine -> (State, Symbol)
tableKey TuringMachine {tState = s, tPosition = p, tTape = t} = (s, t !! p)

move :: Direction -> Position -> Position
move L x = x - 1
move R x = x + 1

updateTape :: Position -> Symbol -> Tape -> Tape
updateTape p s t = left ++ [s] ++ tail right
  where
    (left, right) = splitAt p t

displayState :: Int -> TuringMachine -> IO ()
displayState actions machine = do
    print actions
    print $ tPosition machine
    putStrLn . concatMap show $ tTape machine
