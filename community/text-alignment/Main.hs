import Control.Monad (replicateM)

data Alignment =
    -- | Display text as-is.
      LEFT
    -- | Align text to the right.
    | RIGHT
    -- | Spread the remaining space evenly on both sides of the text.
    -- If the number of remaining spaces is odd, leave the extra space
    -- on the right. Don't actually append space characters to the end
    -- of the line, though.
    | CENTER
    -- | Spread the remaining space evenly between words of the line so
    -- that all lines end on the same column, with the exception that
    -- lines with a single word should remain left-aligned. When
    -- necessary, round the number of spaces to their integer part. For
    -- instance if you need to spread 11 spaces between 5 words, the
    -- theoretical number of spaces between each pair of words is 2.75,
    -- yielding a total number of used spaces of 2.75 - 5.5 - 8.75 -
    -- 11. Round to 2 - 5 - 8 - 11 and the effective number of spaces
    -- in each block is 2 - 3 - 3 - 3.
    | JUSTIFY
    deriving (Show, Read)

main :: IO ()
main = do
    alignment <- readLn
    xs <- readLn >>= flip replicateM getLine
    putStr . unlines $ align alignment xs

align :: Alignment -> [String] -> [String]
align LEFT xs = xs
align JUSTIFY xs = map (concat . pad . words) xs
  where
    pad xs' = zipWith (++) xs' (map (`replicate` ' ') $ spaces totalSpaces (length xs'))
      where
        spaces 0 1 = [0]
        spaces s w = x : spaces (s - x) (w - 1)
          where
            x = s `div` (w - 1)
        totalSpaces = maxLength - charCount xs'
    charCount = length . concat
    maxLength = maximum $ map length xs
align alignment xs = map pad xs
  where
    pad x = (flip replicate ' ' . padAmount alignment . length $ x) ++ x
    padAmount RIGHT = (`subtract` maxLength)
    padAmount CENTER = (`div` 2) . (`subtract` maxLength)
    padAmount _ = error "this should never happen"
    maxLength = maximum $ map length xs
