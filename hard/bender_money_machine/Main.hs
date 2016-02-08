{-
    The Goal

You have managed to find Bender. His circuits are programmed with an algorithm which has made him lose all sense of humor or fun. You decide to return his system to his original behavior.

The first trait of character that you are going to work on is his passion for money. Bender can detect bank notes several miles away.

Your objective is to write a program that will lead him to always find the most money possible, wherever it is.

    Rules

To check that your algorithm works properly, Bender is placed in a room at the entrance of a building. There is a sum of money in each of the rooms that make up this building. Each room has exactly two doors which you can use to get out. Each door leads either to a new room or to the outside of the building.

Bender must collect as much money as possible, by going from room to room, without being able to go back, until he gets out of the building. Thanks to his sensors, Bender can analyze the building right from the outset in order to detect the list of rooms and the money contained in each of them. This analysis of the rooms will be provided as input for your program so that you can implement an algorithm that will make the best decisions.

Each room in the building has a unique number. Each door of a room is marked with a number, which represents the number of the room that it opens, or E if it opens to the outside of the building. Several doors can lead to one room but due to the layout of the building it is impossible for Bender to go through the same room twice. Some doors open in one direction only.

The first room in which Bender finds himself has the number 0.

    Example
    <diagram>
    In this example, Bender pockets $40 by going through the rooms 0, 2, 3 and 5 before leaving the building.

    Game Input

Input
Line 1: the number N of rooms in the building.
N following lines: one line with a room number (integer), a sum of money (integer), the two numbers of the rooms which are accessible (integers or E if the door is an exit). These four values are separated by a white space.
Output
An integer representing the maximum amount of money that Bender can collect by taking a series of doors to reach the outside of the building.
Constraints
0 < N < 10000
Example
Input
15
0 17 1 2
1 15 3 4
2 15 4 5
3 20 6 7
4 12 7 8
5 11 8 9
6 18 10 11
7 19 11 12
8 12 12 13
9 11 13 14
10 13 E E
11 14 E E
12 17 E E
13 19 E E
14 15 E E
Output
88
-}
import Data.List (nub)
import Data.Maybe
import System.IO

-- | Adjacency list.
type Graph = [(Node, [Node])]
type Node = String
type Weight = Int
type Path = [Node]

main :: IO ()
main = do
    (graph, weights) <- readInput stdin
    print $ maximum $ map (cost weights) $ findPaths "0" "E" graph

readInput :: Handle -> IO (Graph, [(Node, Weight)])
readInput = fmap (unzip . map parseLine . tail . lines) . hGetContents

parseLine :: String -> ((Node, [Node]), (Node, Weight))
parseLine line = ((node, nub [a, b]), (node, read weight))
    where
    [node, weight, a, b] = words line

findPaths :: Node -> Node -> Graph -> [Path]
findPaths source target graph
    | source == target = [[target]]
    | otherwise = do
        neighbor <- fromJust $ lookup source graph
        rest <- findPaths neighbor target graph
        return $ source : rest

cost :: [(Node, Weight)] -> Path -> Weight
cost weights path = sum $ map (\node -> fromMaybe 0 $ lookup node weights) path
