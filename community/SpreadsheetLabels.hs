{-
Goal

Spreadsheets commonly use numeric labels for each row, and alphabetic labels for each column. The alphabetic labels follow this pattern:

A, B, C... Z,
AA, AB, AC... AZ
BA, BB, BC... ZZ
AAA, AAB, AAC...

Your program should consume a list of labels and transform each label to the opposite label type:
When a number is provided, output the corresponding alphabetic label.
When an alphabetic label is provided, output the corresponding number.

Keep in mind, the numeric labels are one-indexed (1 = A, 2 = B, etc). There is no zero.
Input
Line 1: an integer N
Line 2: a space-separated string of N labels (mix of numeric and alphabetic labels)
Output
Line 1: a space-separated string of N labels in the order they appeared in the input string, but with each converted to the opposite label type (numeric to alphabetic and vice versa).
Constraints
1 ≤ N ≤ 100
Example
Input
6
1 26 27 52 53 702
Output
A Z AA AZ BA ZZ
-} 
import Data.Char
import Data.Maybe

main :: IO ()
main = interact $ unwords . map convert . tail . words

convert :: String -> String
convert xs
    | isAlpha (head xs) = show $ toIndex xs
    | otherwise = toLabel (read xs)

toIndex :: String -> Int
toIndex xs =
    sum $ zipWith (\x p -> (ord x - ord 'A' + 1) * 26 ^ p) (reverse xs) [0..]

toLabel :: Int -> String
toLabel 0 = ""
toLabel x = toLabel d ++ [fromJust (lookup m table)]
  where
    table = zip [0..] ['A'..'Z']
    (d, m) = (x - 1) `divMod` 26
