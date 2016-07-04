import Control.Applicative ((<*>))
import Data.List (findIndices)

main :: IO ()
main = getLine >>= putStrLn . unscramble

secondLetters :: [Char]
secondLetters = ['B','D'..'Z']

thirdLetters :: [Char]
thirdLetters = ['C','F'..'Z']

fourthLetters :: [Char]
fourthLetters = ['D','H'..'Z']

unscramble :: String -> String
-- unscramble = fourth . third . second . first
unscramble = second 

first :: String -> String
first xs = foldr swap xs . genPairs . findIndices (`elem` secondLetters) $ xs
  where
    genPairs xs =
        take (ceiling $ fromIntegral (length xs) / 2) $
        zip xs (reverse xs)

second :: String -> String
second xs = head . tail . scanr swap xs . genPairs . findIndices (`elem` thirdLetters) $ xs
  where
    genPairs xs = zip xs (tail (cycle xs))

third :: String -> String
third = undefined

fourth :: String -> String
fourth = undefined

swap :: (Int, Int) -> [a] -> [a]
swap (i, j) xs = replaceAtIndex j a $ replaceAtIndex i b xs
  where
    a = xs !! i
    b = xs !! j

-- | Replace a value of a list at a given index.
--
-- http://stackoverflow.com/a/10133429/1988505
replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item:b) where (a, _:b) = splitAt n ls