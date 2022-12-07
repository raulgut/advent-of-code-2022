-----------------------------------------------------------------------------
-- | First part of Day 3
-- 
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.Set (Set, fromList, intersection, elems)
import Data.Char (ord)

-- | obtains the compartments for each rucksack
getGroupBadge :: [String] -> [Char]
getGroupBadge [] = []
getGroupBadge (c1:c2:c3:rest)
  = let badge = intersection (fromList c3) (intersection (fromList c1) (fromList c2))
    in (elems badge) ++ (getGroupBadge rest)

-- | from char to value
getValue c | c >= 'a' && c <= 'z' = (ord c) - (ord 'a') + 1 
getValue c | c >= 'A' && c <= 'Z' = (ord c) - (ord 'A') + 27 

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = map (filter (/= '\r')) . lines $ input
  putStrLn . show . sum . map getValue . getGroupBadge $ entries
