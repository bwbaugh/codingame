import           Data.Char (digitToInt, intToDigit)
import           Data.List (sort, sortBy)

data Symbol
    = Digit Int
    | Dot
    | Minus
    deriving (Eq, Ord, Show)

main :: IO ()
main = getLine >> getLine >>= putStrLn . solve . parse

parse :: String -> [Symbol]
parse = map parseSymbol . concat . words

parseSymbol :: Char -> Symbol
parseSymbol '.' = Dot
parseSymbol '-' = Minus
parseSymbol x = Digit (digitToInt x)

showSymbol :: Symbol -> Char
showSymbol Dot = '.'
showSymbol Minus = '-'
showSymbol (Digit x) = intToDigit x

solve :: [Symbol] -> String
solve = map showSymbol . clean . optimize

optimize :: [Symbol] -> [Symbol]
optimize xs | Minus `elem` xs = Minus : minimize (filter (/= Minus) xs)
optimize xs = maximize xs

maximize :: [Symbol] -> [Symbol]
maximize xs
    | Dot `elem` xs = maxDot . sortBy (flip compare) . filter (/= Dot) $ xs
    | otherwise = sortBy (flip compare) xs

maxDot :: [Symbol] -> [Symbol]
maxDot [x] = [Dot, x]
maxDot (x:xs) = x : maxDot xs

minimize :: [Symbol] -> [Symbol]
minimize xs
    | Dot `elem` xs = minDot . sort . filter (/= Dot) $ xs
    | otherwise = sort xs

minDot :: [Symbol] -> [Symbol]
minDot (x:xs) = x : Dot : xs

clean :: [Symbol] -> [Symbol]
clean = fixEmpty . fixNegativeZero . fixDot . removeZeros

removeZeros :: [Symbol] -> [Symbol]
removeZeros =
    reverse . dropWhile (== Digit 0) . reverse . dropWhile (== Digit 0)

fixDot :: [Symbol] -> [Symbol]
fixDot [] = []
fixDot [Dot] = []
fixDot (x:xs) = x : fixDot xs

fixNegativeZero :: [Symbol] -> [Symbol]
fixNegativeZero [Minus, Digit 0] = [Digit 0]
fixNegativeZero xs = xs

fixEmpty :: [Symbol] -> [Symbol]
fixEmpty [] = [Digit 0]
fixEmpty xs = xs
