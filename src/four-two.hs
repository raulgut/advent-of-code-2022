-----------------------------------------------------------------------------
-- | Second part of Day 4
-- 
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (Parser)
import Text.Parsec.Error(ParseError (..))
import Text.Parsec.Char (digit, char)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Prim (parse)
import Data.Set (Set, fromList, intersection)

-- | Parses sections
extractSections :: String -> (Set Int, Set Int)
extractSections str
  = case (parse sectionsParser "" str) of
      Left err -> error . show $ err
      Right (s1,s2) -> (s1,s2)

-- | Sections separated by comma
sectionsParser :: Parser (Set Int,Set Int)
sectionsParser =
 do s1 <- sectionParser
    char ','
    s2 <- sectionParser
    return $ (s1,s2)

-- | Sections between hyphen
sectionParser :: Parser (Set Int)
sectionParser =
 do b1 <- (many1 digit)
    char '-'
    b2 <- (many1 digit)
    return . fromList $ [(read b1)..(read b2)]

-- | Checks if one set is overlapped with the other or 
--   viceversa
isOverlap :: (Set Int, Set Int) -> Bool
isOverlap (s1,s2) = (not . null $ intersection s1 s2) || (not . null $ intersection s2 s1)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (filter isOverlap . map extractSections . map (filter (/= '\r')) . lines $ input)
  putStrLn . show . length $ entries
