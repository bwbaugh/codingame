import           Control.Monad   (replicateM_)
import           Data.List.Split (splitOn)
import           System.IO
    ( BufferMode (NoBuffering)
    , hSetBuffering
    , stdout
    )

-- XXX: Not using Rational because it raises exceptions for zero
-- denominator.
data Fraction = Fraction Integer Integer

instance Show Fraction where
    show (Fraction num denom) = show num ++ "/" ++ show denom

data Answer = DivisionByZero | Reduced Integer | Mixed Integer Fraction

instance Show Answer where
    show DivisionByZero = "DIVISION BY ZERO"
    show (Reduced n) = show n
    show (Mixed 0 fraction) = show fraction
    show (Mixed whole fraction) = show whole ++ " " ++ show fraction

main :: IO ()
main =
    hSetBuffering stdout NoBuffering >>
    readLn >>=
    flip replicateM_ testCase

testCase :: IO ()
testCase = readFraction >>= print . solve . simplify

readFraction :: IO Fraction
readFraction = fmap parseFraction getLine

parseFraction :: String -> Fraction
parseFraction s = let [n, d] = splitOn "/" s in Fraction (read n) (read d)

simplify :: Fraction -> Fraction
simplify (Fraction 0 0) = Fraction 0 0
simplify (Fraction n d) = fraction
  where
    divisor = gcd n d
    n' = n `div` divisor
    d' = d `div` divisor
    fraction
        | n' < 0 && d' < 0 = Fraction (-n') (-d')
        | otherwise        = Fraction n' d'

solve :: Fraction -> Answer
solve (Fraction _ 0)    = DivisionByZero
solve (Fraction n 1)    = Reduced n
solve (Fraction n (-1)) = Reduced (-n)
solve (Fraction n d)
    | n < 0     = Mixed   q  (Fraction (-r) d)
    | d < 0     = Mixed (-q) (Fraction r (-d))
    | otherwise = Mixed   q  (Fraction r d)
  where
    (q, r) = abs n `divMod` abs d
