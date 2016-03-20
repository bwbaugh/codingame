{-
Goal

Terry loves candies.
He just got a new batch of candies and he's wondering about how he
should eat them.

Terry is trying to eat those N candies and he can put up to K candies
in his mouth at once.
What are the possibilities for Terry to eat his candies ?

For example, let's say Terry wants to eat 3 candies and that he can put
in his mouth up to 2 candies :
He can just eat 1 candy three times in a row.
He can eat 1 candy, and then 2.
Or, he can also eat 2, and then eat 1.

The output should then be :
1 1 1
1 2
2 1

Input

Two integers separated by a space: N the number of candies, K the size
of Terry's mouth.

Output

The possibilities need to be sorted using the lexicographical order.
For each possibility :
A line of integers separated by spaces : The possibility, i.e. the
numbers of candy that Terry puts in his mouth at each step.

Constraints

N ≤ 10
K ≤ 10

Example

Input

1 1

Output

1
-}
import Control.Monad

main :: IO ()
main = do
    [n, k] <- fmap (map read . words) getLine
    putStr . unlines . map (unwords . map show) $ candy n k

candy :: Int -> Int -> [[Int]]
candy n k = do
    x <- if n > 0 then [1..k] else []
    let n' = n - x
    guard (n' >= 0)
    xs <- if n' > 0 then candy n' k else [[]]
    return (x : xs)
