-----------------------------------------------------------------------------
-- | Second part of Day 2
-- 
-----------------------------------------------------------------------------

import System.Environment (getArgs)

-- | Possible moves
data Hand = Rock
          | Paper
          | Scissors
          deriving Show

-- | obtain the play from A|B|C and X|Y|Z
getPlay :: Char -> Hand
getPlay 'A' = Rock
getPlay 'B' = Paper
getPlay 'C' = Scissors
getPlay 'X' = Rock
getPlay 'Y' = Paper
getPlay 'Z' = Scissors

-- | get the line match
getMatch :: String -> (Hand,Char)
getMatch (p1:' ':p2:[]) = (getPlay p1, p2)

-- | get the score of the match
getScore :: (Hand,Char) -> Int
getScore (Rock,'X') = 3
getScore (Rock,'Y') = 4
getScore (Rock,'Z') = 8
getScore (Paper,'X') = 1
getScore (Paper,'Y') = 5
getScore (Paper,'Z') = 9
getScore (Scissors,'X') = 2
getScore (Scissors,'Y') = 6
getScore (Scissors,'Z') = 7

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (map (getScore . getMatch) . map (filter (/= '\r')) . lines $ input)
  putStrLn . show . sum $ entries
