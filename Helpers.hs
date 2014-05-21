module Helpers where

import Text.ParserCombinators.Parsec
import Data.Colour.SRGB
import Data.Colour.Names
import Data.Functor.Identity
import Data.Maybe (fromMaybe)

import qualified Text.Parsec.Prim as P

import qualified NewDataTypes as DT

----
---- helpers
----

upgradeNames :: Maybe DT.Upgrades -> [String]
upgradeNames Nothing = []
upgradeNames (Just u) = s' ++ d' ++ b'
  where
    s' = map DT.getShotUpgradeName $ DT.getShotUpgrades u
    d' = map DT.getShieldName $ DT.getShieldUpgrades u
    b' = map DT.getBombName $ DT.getBombUpgrades u

singleNames :: DT.NEList DT.Single -> [String]
singleNames = map DT.getSingleName . toList

groupNames :: DT.NEList DT.Group -> [String]
groupNames =  map DT.getGroupName . toList

randomNames :: [DT.Random] -> [String]
randomNames =  map DT.getRandomName

timelineNames :: [DT.Random] -> DT.NEList DT.Group -> DT.NEList DT.Single -> DT.Elements -> ([String], [String], [String], [String])
timelineNames r g s u = (r',g',s',u')
  where
    r' = randomNames r
    g' = groupNames g
    s' = singleNames s
    u' = upgradeNames $ DT.getUpgrades u

-- quote definition
quote :: Parser String
quote = string "\""

toList :: DT.NEList a -> [a]
toList (DT.NEList x xs) = x : xs

antagNames :: DT.Elements -> [String]
antagNames = map DT.getAntagName . toList . DT.getAntags

bulletNames :: DT.Elements -> [String]
bulletNames = map DT.getBulletName . DT.getBullets

str :: Parser String
str = try $ do
  s <- between quote quote (many alphaNum)
  spaces
  return $ s

natify :: String -> DT.Nat
natify = DT.toNat . read

readColor :: String -> Colour Double
readColor color = fromMaybe (error $ "invalid color: " ++ color) $ readColourName color

makeColor :: String -> DT.Colour
makeColor = DT.Colour . sRGB24show . readColor

-- get line
line :: Parser String
line = do
 ln <- manyTill anyChar newline
 spaces
 return ln

-- parse digits
digit' :: Parser DT.Nat
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
  case (sourceColumn p) == (sourceColumn innerPos) of
    True -> pzero
    False -> return $ []

-- options
options :: [(String,a)] -> Parser a
options l = choice $ map (\(s,r) -> do { _ <- string s; return r }) l

-- check if columns are inline
inline :: SourcePos -> P.ParsecT [tok] u Identity [a]
inline p = do
  a <- getPosition
  if (sourceColumn p) == (sourceColumn a) then
    return []
  else
    pzero

-- check for same indent
sameIndent :: SourcePos -> SourcePos -> Bool
sameIndent a b = (sourceColumn a) == (sourceColumn b)

-- check for greater indent
greaterIndent :: SourcePos -> SourcePos -> Bool
greaterIndent a b = (sourceColumn a) > (sourceColumn b)
