-----------------------------------------------------------------------------
-- | Second part of Day 1
-- 
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.List (groupBy, maximum)
import Data.MultiSet (MultiSet, fromList, insert, delete, elems)

-- | update the Set if it is a top 3 Elf
updateSet :: MultiSet Int -> Int -> MultiSet Int 
updateSet mySet e 
  = let minMySet = minimum mySet
    in if e > minMySet 
       then insert e . delete minMySet $ mySet 
       else mySet

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (map sum . map (map read) . filter (/= [""]) . groupBy (\x y -> (not . null $ x) && (not . null $ y)) . map (filter (/= '\r')) . lines $ input) :: [Int]
  putStrLn . show . sum . elems . foldl updateSet (fromList [0,0,0]) $ entries
