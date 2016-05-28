import System.IO
import Control.Monad

main :: IO ()
main = readLn >>= putStr . unlines . fixWithPeriod . triforce
  where
    fixWithPeriod ((' ':xs):xxs) = ('.':xs):xxs

triforce :: Int -> [String]
triforce n = map (leftPad n) (triangle n) ++ secondRow n

triangle :: Int -> [String]
triangle n = zipWith (++) (pads n) stars
  where
    stars = map (`replicate` '*') $ take n [1,3..]

pads :: Int -> [String]
pads n = map (padForRow n) [1..n]

padForRow :: Int -> Int -> String
padForRow n k = replicate (n - k) ' '

leftPad :: Int -> String -> String
leftPad n x = replicate n ' ' ++ x

secondRow :: Int -> [String]
secondRow n = zipWith (++) t s
  where
    t = triangle n
    s = zipWith (++) (pads (n + 1)) t
