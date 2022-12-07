-----------------------------------------------------------------------------
-- | First part of Day 1
-- 
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.List (groupBy, maximum)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (map sum . map (map read) . filter (/= [""]) . groupBy (\x y -> (not . null $ x) && (not . null $ y)) . map (filter (/= '\r')) . lines $ input) :: [Int]
  putStrLn . show . maximum $ entries
