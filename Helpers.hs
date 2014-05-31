module Helpers where


import Language
import Text.ParserCombinators.Parsec
import Data.Colour.SRGB
import Data.Colour.Names
import Data.Functor.Identity
import Data.Maybe (fromMaybe)

import qualified Text.Parsec.Prim as P
import qualified DAST as DAST

----
---- helpers
----

upgradeNames :: Maybe DAST.Upgrades -> [String]
upgradeNames Nothing = []
upgradeNames (Just u) = s' ++ d' ++ b'
  where
    s' = map DAST.getShotUpgradeName $ DAST.getShotUpgrades u
    d' = map DAST.getShieldName $ DAST.getShieldUpgrades u
    b' = map DAST.getBombName $ DAST.getBombUpgrades u

singleNames :: DAST.NEList DAST.Single -> [String]
singleNames = map DAST.getSingleName . toList

groupNames :: [DAST.Group] -> [String]
groupNames =  map DAST.getGroupName

randomNames :: [DAST.Random] -> [String]
randomNames =  map DAST.getRandomName

timelineNames :: [DAST.Random] -> [DAST.Group] -> DAST.NEList DAST.Single -> DAST.Elements -> ([String], [String], [String], [String])
timelineNames r g s u = (r',g',s',u')
  where
    r' = randomNames r
    g' = groupNames g
    s' = singleNames s
    u' = upgradeNames $ DAST.getUpgrades u

-- quote definition
quote :: Parser String
quote = string "\""

toList :: DAST.NEList a -> [a]
toList (DAST.NEList x xs) = x : xs

antagNames :: DAST.Elements -> [String]
antagNames = map DAST.getAntagName . toList . DAST.getAntags

bulletNames :: DAST.Elements -> [String]
bulletNames = map DAST.getBulletName . DAST.getBullets

str :: Parser String
str = try $ do
  s <- between quote quote (many alphaNum)
  spaces
  return $ s

natify :: String -> DAST.Nat
natify = DAST.toNat . read

readColor :: String -> Colour Double
readColor color = fromMaybe (error $ "invalid color: " ++ color) $ readColourName color

makeColor :: String -> DAST.Colour
makeColor = DAST.Colour . sRGB24show . readColor

-- get line
line :: Parser String
line = do
 ln <- manyTill anyChar newline
 spaces
 return ln

-- parse digits
digit' :: Parser DAST.Nat
digit' = do
  d <- many1 digit
  spaces
  return $ natify d

-- display error
err :: P.Stream s m t => String -> String -> P.ParsecT s u m a
err a b = unexpected ("--> " ++ a ++ " not valid: \"" ++ b ++ "\"")

notFoundError :: P.Stream s m t => String -> String -> P.ParsecT s u m a
notFoundError x a = unexpected ("--> the \"" ++ x ++ "\": \"" ++ a ++ "\" does not exist.")

antagNotFoundError :: P.Stream s m t => String -> P.ParsecT s u m a
antagNotFoundError a = notFoundError "antag" a

bulletNotFoundError :: P.Stream s m t => String -> P.ParsecT s u m a
bulletNotFoundError a = notFoundError "bullet" a

singleNotFoundError :: P.Stream s m t => String -> P.ParsecT s u m a
singleNotFoundError a = notFoundError "single" a

whatInvalidError :: P.Stream s m t => String -> P.ParsecT s u m a
whatInvalidError a = unexpected ("--> " ++ a ++ " is not a valid \'what\'")

ipInvalidError :: P.Stream s m t => String -> String -> P.ParsecT s u m a
ipInvalidError r c = unexpected ("--> initial position error, row: " ++ r ++ ", column: " ++ c)

-- indent
indented :: Show tok => SourcePos -> P.ParsecT [tok] u Data.Functor.Identity.Identity [a]
indented p = (eof >> return []) <|> do
  innerPos <- getPosition
  if sameIndent p innerPos then pzero else return []

-- options
options :: [(String,a)] -> Parser a
options l = choice $ map (\(s,r) -> do { _ <- reserved s; return r }) l

-- check if columns are inline
inline :: SourcePos -> P.ParsecT [tok] u Identity [a]
inline p = do
  a <- getPosition
  if sameIndent a p then return [] else pzero

-- check for same indent
sameIndent :: SourcePos -> SourcePos -> Bool
sameIndent a b = (sourceColumn a) == (sourceColumn b)

-- check for greater indent
greaterIndent :: SourcePos -> SourcePos -> Bool
greaterIndent a b = (sourceColumn a) > (sourceColumn b)
