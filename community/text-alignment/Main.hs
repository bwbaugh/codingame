import Control.Monad (replicateM)

data Alignment = LEFT | RIGHT | CENTER | JUSTIFY deriving (Show, Read)

type WordString = String
type Line = String

main :: IO ()
main = do
    alignment <- readLn
    xs <- readLn >>= flip replicateM getLine
    putStr . unlines $ align alignment xs

align :: Alignment -> [Line] -> [Line]
align LEFT xs = xs
align JUSTIFY xs = map (concat . jpad maxLength . words) xs
  where
    maxLength = maximum $ map length xs
align alignment xs = map pad xs
  where
    pad x = (flip replicate ' ' . padAmount alignment . length $ x) ++ x
    padAmount RIGHT = (`subtract` maxLength)
    padAmount CENTER = (`div` 2) . (`subtract` maxLength)
    padAmount _ = error "this should never happen"
    maxLength = maximum $ map length xs

jpad :: Int -> [WordString] -> [WordString]
jpad maxLength xs = zipWith (++) xs spaces
  where
    spaces = map (`replicate` ' ') $ jspaces totalSpaces (length xs)
    totalSpaces = maxLength - charCount xs
    charCount = length . concat

jspaces :: Int -> Int -> [Int]
jspaces 0 1 = [0]
jspaces s w = let x = s `div` (w - 1) in x : jspaces (s - x) (w - 1)
