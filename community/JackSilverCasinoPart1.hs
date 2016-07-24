{-# OPTIONS_GHC -Wall -Werror #-}
type Money = Integer

data Play = Play Ball Call deriving (Show)

type Ball = Integer

data Call = EVEN | ODD | PLAIN Ball deriving (Read, Show)

main :: IO ()
main = do
    _ <- getLine
    startingMoney <- readLn
    plays <- fmap (map parseRound . lines) getContents
    print $ foldr step startingMoney plays

parseRound :: String -> Play
parseRound s = case words s of
    [ball, call]         -> Play (read ball) (read call)
    [ball, call, number] -> Play (read ball) (read (unwords [call, number]))
    _                    -> error "couldn't parse round"

step :: Play -> Money -> Money
step _ 0 = 0
step play money = if isWin play then win play money else lose money

isWin :: Play -> Bool
isWin (Play 0 EVEN)              = False
isWin (Play ball EVEN)           = even ball
isWin (Play ball ODD)            = odd  ball
isWin (Play ball (PLAIN number)) = ball == number

win :: Play -> Money -> Money
win (Play _ EVEN)      money = money + bet money
win (Play _ ODD)       money = money + bet money
win (Play _ (PLAIN _)) money = money + (bet money * plainOdds)

plainOdds :: Integer
plainOdds = 35

lose :: Money -> Money
lose money = max 0 (money - bet money)

bet :: Money -> Money
bet = ceiling . (/ (4 :: Double)) . fromIntegral
