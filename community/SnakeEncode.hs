import Control.Monad (replicateM)

main :: IO ()
main = do
    n <- readLn :: IO Int
    x <- readLn :: IO Int
    replicateM n getLine >>= putStr . unlines . multipleSnakeEncode x

multipleSnakeEncode :: Int -> [String] -> [String]
multipleSnakeEncode x = last . take x . tail . iterate snakeEncode

snakeEncode :: [String] -> [String]
snakeEncode xss = go (length xss - 1, 0) xss

go :: (Int, Int) -> [String] -> [String]
go pos@(row, col) xss
    | pos' == endPos n = xss'
    | otherwise        = go pos' xss'
  where
    n = length xss
    pos' = nextPos (length xss) pos
    xss' = swap pos pos' xss

nextPos :: Int -> (Int, Int) -> (Int, Int)
nextPos n (row, col)
    -- Starting position always moves up.
    | row == (n - 1) && col == 0 = (row - 1, 0)
    -- Move to the right.
    | row `elem` [0, n - 1]      = (row, col + 1)
    -- Move up.
    | col `rem` 2 == 0           = (row - 1, col)
    -- Move down.
    | otherwise                  = (row + 1, col)

swap :: (Int, Int) -> (Int, Int) -> [[a]] -> [[a]]
swap (r1, c1) (r2, c2) xs = xs''
  where
    a = xs !! r1 !! c1
    b = xs !! r2 !! c2
    xs'  = replaceAtIndex r1 (replaceAtIndex c1 b (xs !! r1)) xs
    xs'' = replaceAtIndex r2 (replaceAtIndex c2 a (xs !! r2)) xs'

-- http://stackoverflow.com/a/10133429/1988505
replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item:b) where (a, _:b) = splitAt n ls

endPos :: Int -> (Int, Int)
endPos 1 = (0, 0)
endPos n
    -- Bottom right.
    | n `rem` 2 == 0 = (n - 1, n - 1)
    -- Top right.
    | otherwise      = (0,     n - 1)
