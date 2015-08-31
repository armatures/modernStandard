main = do  
    putStrLn "What are the two players' ordered scores?"  
    scores <- getLine  
    putStrLn (scores ++ ", according to you!")

data Score = Score (Int, Int) deriving (Show)

parseScore :: String -> Score
parseScore a = Score $ firstTwo $ getScoreList a

firstTwo :: [Int] -> (Int, Int)
firstTwo (a:b:xs) = (a,b)

getScoreList :: String -> [Int]
getScoreList s = map parseInt (words s)

parseInt :: String -> Int
parseInt a = read a :: Int

numWord :: Int -> String
numWord 1 = "One"
numWord 2 = "Two"
numWord 3 = "Three"
numWord 4 = "Four"
numWord 5 = "Five"
numWord 6 = "Six"
numWord 7 = "Seven"
numWord 8 = "Eight"
numWord 9 = "Nine"

modern :: Score -> String
modern (Score (a,b))
              | a < b     = unwords $ [deuce a, duplicity (a-b), "out"]
              | a > b     = unwords $ [deuce b, duplicity (a-b), "in"]
              | otherwise = deuce a
                where deuce s = numWord s ++ "deuce"

duplicity :: Int -> String
duplicity i = dupName $ abs i
        where dupName 1 = "once"
              dupName 2 = "twice"
              dupName 3 = "thrice"
              dupName a = numWord a


