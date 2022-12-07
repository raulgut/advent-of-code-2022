-----------------------------------------------------------------------------
-- | First part of Day 2
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
getMatch :: String -> (Hand,Hand)
getMatch (p1:' ':p2:[]) = (getPlay p1, getPlay p2)

-- | get the score of the match
getScore :: (Hand,Hand) -> Int
getScore (Rock,Rock) = 4
getScore (Rock,Paper) = 8
getScore (Rock,Scissors) = 3
getScore (Paper,Rock) = 1
getScore (Paper,Paper) = 5
getScore (Paper,Scissors) = 9
getScore (Scissors,Rock) = 7
getScore (Scissors,Paper) = 2
getScore (Scissors,Scissors) = 6

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (map (getScore . getMatch) . map (filter (/= '\r')) . lines $ input)
  putStrLn . show . sum $ entries
