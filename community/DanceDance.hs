{-
Goal
====

Your program must automate the create of maps for the new game "Dance
Dance CodinGame". In this game, the player must push 4 keys in rhythm.

One map is composed of lines of 4 characters each: a zero (0) indicates
that the corresponding key is released, a cross (X) indicates that the
corresponding key is pushed.

You are given the patterns of lines Pattern and their tempo Tempo. You
must reproduce the pattern every Tempo lines.

If one line has no pattern, it is composed of 4 zeros: 0000.

If one line has multiple patterns, you must accumulate cross. For
example, XX00 and X0X0 becomes XXX0.

Warning: the map starts from the bottom to the top!

Input
=====

Line 1: The length L of the map.
Line 2: The number N of pair Pattern Tempo.
N next lines: A string Pattern and a number Tempo.

Output
======

L lines representing the map.

Constraints
==========

0 < L < 100
0 < N < 10
0 < Tempo < 100

Example
=======

Input
----

7
2
X000 2
00XX 3

Output
-----

0000
X0XX
0000
X000
00XX
X000
0000
-}
data LinePattern = LinePattern {
      getPattern :: Pattern
    , getTempo :: Tempo
    } deriving (Show)
data Key = Released | Pushed deriving (Show)
-- TODO(bwbaugh|2016-03-11): Convert to product type with Monoid instance.
type Pattern = [Key]
type Tempo = Int

main :: IO ()
main = do
    duration <- readLn
    _ <- getLine
    patterns <- fmap (map (parseLinePattern . words) . lines) getContents
    putStr . unlines . reverse . map (map showKey) $ genLevel patterns duration

parseLinePattern :: [String] -> LinePattern
parseLinePattern [keys, tempo] = LinePattern (map parseKey keys) (read tempo)

parseKey :: Char -> Key
parseKey '0' = Released
parseKey 'X' = Pushed

showKey :: Key -> Char
showKey Released = '0'
showKey Pushed = 'X'

genLevel :: [LinePattern] -> Int -> [Pattern]
genLevel ps duration = take duration $ map (genPattern ps) [1..]

genPattern :: [LinePattern] -> Int -> Pattern
genPattern ps index = foldr joinPattern empty $ map getPattern activePatterns
  where
    empty = replicate 4 Released
    activePatterns = filter isActive ps
    isActive (LinePattern _ t) = index `rem` t == 0

joinPattern :: Pattern -> Pattern -> Pattern
joinPattern = zipWith joinKey

joinKey :: Key -> Key -> Key
joinKey Pushed _ = Pushed
joinKey _ Pushed = Pushed
joinKey _ _ = Released
