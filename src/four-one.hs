-----------------------------------------------------------------------------
-- | First part of Day 4
-- 
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (Parser)
import Text.Parsec.Error(ParseError (..))
import Text.Parsec.Char (digit, char)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Prim (parse)
import Data.Set (Set, fromList, isSubsetOf)

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

-- | Checks if one set is fully contained in the other or 
--   viceversa
isContained :: (Set Int, Set Int) -> Bool
isContained (s1,s2) = (isSubsetOf s1 s2) || (isSubsetOf s2 s1)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (filter isContained . map extractSections . map (filter (/= '\r')) . lines $ input)
  putStrLn . show . length $ entries
