-----------------------------------------------------------------------------
-- | First part of Day 3
-- 
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.Set (Set, fromList, intersection, elems)
import Data.Char (ord)

-- | obtains the compartments for each rucksack
getCompartmentsDiff :: String -> [Char]
getCompartmentsDiff items
  = let litems = length items
        (c1,c2) = splitAt (div litems 2) items
    in elems $ intersection (fromList c1) (fromList c2)

-- | from char to value
getValue c | c >= 'a' && c <= 'z' = (ord c) - (ord 'a') + 1 
getValue c | c >= 'A' && c <= 'Z' = (ord c) - (ord 'A') + 27 

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = map getCompartmentsDiff . map (filter (/= '\r')) . lines $ input
  putStrLn . show . sum . concatMap (map getValue) $ entries
