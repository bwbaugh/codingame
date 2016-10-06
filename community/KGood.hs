{-# OPTIONS_GHC -Wall #-}

main :: IO ()
main = do
    xs <- getLine
    k <- readLn
    print (kgood xs k)

kgood :: [a] -> Int -> Int
kgood = undefined
