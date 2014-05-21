module NewLogicParser where

import Helpers

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Data.Functor.Identity
import qualified Text.Parsec.Prim as P
import qualified Text.ParserCombinators.Parsec.Token as T
import qualified NewDataTypes as DT
import Control.Applicative hiding ((<|>), many)

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) a b = (++) <$> a <*> b

(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) a b = (:) <$> a <*> b


def :: LanguageDef g
def = emptyDef {
  reservedNames = [
    "logic",
    "base",
    "protag",
    "weight",
    "bombCount",
    "level",
    "group",
    "shield",
    "shot",
    "name",
    "using",
    "lanes",
    "bomb",
    "enterFrom",
    "timeline",
    "timestamp",
    "count",
    "score",
    "time",
    "boss",
    "progression",
    "mt",
    "lt",
    "rt",
    "grid",
    "lives",
    "iP",
    "single",
    "time"
  ]
}

-- lexer setup
makuLexer :: T.TokenParser s
makuLexer     = T.makeTokenParser def

reserved :: String -> P.ParsecT String u Data.Functor.Identity.Identity ()
reserved      = T.reserved makuLexer

--
-- helper parsers
--

--makuCode :: Parser DT.Logic
--makuCode = try $ logic

-- logic parser
logic :: [String] -> [String] -> Double -> DT.Elements -> Parser DT.Logic
logic b a d u = do
  sP <- getPosition
  reserved "logic"
  _ <- indented sP
  nP <- getPosition
  r <- randoms a
  _ <- inline nP
  s <- singles a d
  _ <- inline nP
  g <- groups a d
  _ <- inline nP
  l <- levels b (timelineNames r g s u) (singleNames s)
  return $ DT.Logic r s g l


lifeCount :: Parser DT.LifeCount
lifeCount = try $ do
  l <- lifeNum <|> infinite
  return $ l

lifeNum :: Parser DT.LifeCount
lifeNum = try $ do
  n <- readNat "lives"
  return $ DT.Countable n

infinite :: Parser DT.LifeCount
infinite = try $ do
  reserved "lives"
  reserved "infinite"
  return $ DT.Infinite

bombs :: Parser DT.Nat
bombs = readNat "bombs"

singles :: [String] -> Double -> Parser (DT.NEList DT.Single)
singles a d = try $ do
  (s:ss) <- many $ single a d
  spaces
  return $ DT.NEList s ss

randoms :: [String] -> Parser [DT.Random]
randoms a = try $ do
  r <- many $ random a
  spaces
  return $ r

random :: [String] -> Parser DT.Random
random a = try $ do
  sP <- getPosition
  reserved "random"
  _  <- indented sP
  nP <- getPosition
  n  <- name
  _  <- inline nP
  u  <- using a
  _  <- inline nP
  s  <- side
  _  <- inline nP
  c  <- counter
  _  <- inline nP
  w  <- wait
  return $ DT.Random n u s c w

side :: Parser DT.Side
side = try $ do
  reserved "side"
  s <- options [
    ("left", DT.LeftSide),
    ("right", DT.RightSide),
    ("top", DT.TopSide),
    ("bottom", DT.BottomSide)]
  spaces
  return $ s

wait :: Parser DT.Nat
wait = readNat "wait"

single :: [String] -> Double -> Parser DT.Single
single a d = try $ do
  sP <- getPosition
  reserved "single"
  _  <- indented sP
  nP <- getPosition
  n  <- name
  _  <- inline nP
  u  <- using a
  _  <- inline nP
  i  <- iP d
  return $ DT.Single n u i


iP :: Double -> Parser DT.IP
iP d = try $ do
  reserved "iP"
  r <- digit'
  spaces
  c <- digit'
  spaces
  if (DT.unNat r) < 0
     || (DT.unNat c) < 0
     || (DT.unNat r) > d
     || (DT.unNat c) > d
  then ipInvalidError (show $ DT.unNat r) (show $ DT.unNat c)
  else return $ DT.IP r c

groups :: [String] -> Double -> Parser (DT.NEList DT.Group)
groups a d = try $ do
  (g:gs) <- many $ group a d
  spaces
  return $ DT.NEList g gs

group :: [String] -> Double -> Parser DT.Group
group a d = try $ do
  sP <- getPosition
  reserved "group"
  _  <- indented sP
  nP <- getPosition
  n  <- name
  _  <- inline nP
  u  <- using a
  _  <- inline nP
  l  <- lanes
  _  <- inline nP
  c  <- counter
  _  <- inline nP
  i  <- iP d
  return $ DT.Group n u l c i

-- parse protags
protag :: [String] -> Parser (Maybe DT.Protag)
protag q = try $ do
  sP <- getPosition -- starting position
  reserved "protag"
  _  <- indented sP       -- set indent
  nP <- getPosition -- new position
  if greaterIndent sP nP
  then return $ Nothing
  else do
    clr <- color
    _   <- inline nP         -- preserve indent
    x   <- mt nP q
    y   <- option (Just $ DT.TL Nothing) (lt nP q)
    z   <- option (Just $ DT.TR Nothing) (rt nP q)
    _   <- inline nP
    w   <- weight
    _   <- inline nP
    b   <- option (DT.Nat 0) bombs
    _   <- inline nP
    l   <- lifeCount
    return $ Just $ DT.Protag clr (DT.Traditional x y z) w b l

data PW = PW SourcePos DT.Protag

mt :: SourcePos -> [String] -> Parser (Maybe DT.Turret)
mt nP b = turret "mt" b nP

lt :: SourcePos -> [String] -> Parser (Maybe DT.Turret)
lt nP b = turret "lt" b nP

rt :: SourcePos -> [String] -> Parser (Maybe DT.Turret)
rt nP b = turret "rt" b nP

turret :: String -> [String] -> SourcePos -> Parser (Maybe DT.Turret)
turret x b nP = try $ do
  _ <- inline nP
  reserved x
  s <- shot b
  case x of
    "mt" -> return $ Just $DT.TM (Just s)
    "lt" -> return $ Just $ DT.TL (Just s)
    "rt" -> return $ Just $DT.TR (Just s)

shot :: [String] -> Parser DT.Shot
shot b = try $ do
  n <- between quote quote (many alphaNum)
  spaces
  c <- digit'
  spaces
  if elem n b
  then return $ DT.Shot n (DT.ShotType c)
  else bulletNotFoundError n

levels :: [String] -> ([String],[String],[String],[String]) -> [String] -> Parser (DT.NEList DT.Level)
levels b t s = do
  (l:ls) <- many $ level b t s
  spaces
  return $ DT.NEList l ls

-- NEED TO FIX
level :: [String] -> ([String],[String],[String],[String]) -> [String] -> Parser DT.Level
level b q x = do
  sP <- getPosition
  reserved "level"
  _  <- indented sP
  nP <- getPosition
  (Just p)  <- protag b
  _  <- inline nP
  (Just t)  <- timeline q
  _  <- inline nP
  s  <- boss x <|> score <|> time
  return $ DT.Level p t s

boss :: [String] -> Parser DT.WinCondition
boss x = do
  reserved "boss"
  d <- str
  spaces
  if elem d x
  then return $ DT.Boss "boss"
  else singleNotFoundError d

score :: Parser DT.WinCondition
score = readNatWithType DT.Score "score"

time :: Parser DT.WinCondition
time = readNatWithType DT.Time "time"

timeline :: ([String],[String],[String],[String]) -> Parser (Maybe DT.Timeline)
timeline x = try $ do
  sP <- getPosition
  reserved "timeline"
  _  <- indented sP
  nP <- getPosition
  if greaterIndent sP nP
  then return $ Nothing
  else do
    t <- timestamps x
    return $ Just $ DT.Timeline t

timestamps :: ([String],[String],[String],[String]) -> Parser (DT.NEList DT.Timestamp)
timestamps x = try $ do
  (t:ts) <- many $ timestamp x
  return $ DT.NEList t ts

timestamp :: ([String],[String],[String],[String]) -> Parser DT.Timestamp
timestamp (r,g,s,u) = try $ do
  reserved "timestamp"
  b <- digit'
  spaces
  n <- str
  spaces
  d <- duration
  if elem n r
  then return $ DT.Timestamp 3 b n d
  else
    if elem n g
    then return $ DT.Timestamp 0 b n d
    else
      if elem n s
      then return $ DT.Timestamp 2 b n d
      else
        if elem n u
        then return $ DT.Timestamp 1 b n d
        else whatInvalidError n

duration :: Parser (Maybe DT.Duration)
duration = try $ do
  d <- optionMaybe float
  case d of
    Just x -> return $ Just $ DT.Duration $ x
    Nothing -> return $ Nothing

-- adapted from: https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/parsing-floats-with-parsec
float :: Parser Float
float = try $ do
  d <- fmap rd $ many1 digit <++> decimal
  spaces
  return $ d
    where rd       = read :: String -> Float
          decimal  = option "" $ char '.' <:> (many1 digit)

color :: Parser DT.Colour
color = try $ do
  reserved "color"
  c <- line
  return $ makeColor c

-- parse weight
weight :: Parser DT.Nat
weight = readNat "weight"

name :: Parser String
name = readStr "name"

using :: [String] -> Parser String
using a = try $ do
  reserved "using"
  s <- str
  spaces
  if elem s a
  then return $ s
  else antagNotFoundError s

lanes :: Parser DT.Nat
lanes = readNat "lanes"

counter :: Parser DT.Nat
counter = readNat "count"

readStr :: String -> Parser String
readStr n = try $ do
  reserved n
  s <- str
  spaces
  return $ s

readNat :: String -> Parser DT.Nat
readNat n = try $ do
  reserved n
  d <- digit'
  spaces
  return $ d

readNatWithType :: (DT.Nat -> a) -> String -> Parser a
readNatWithType t n = try $ do
  reserved n
  d <- digit'
  spaces
  return $ t $ d

readStringWithType :: (String -> a) -> String -> Parser a
readStringWithType t n = try $ do
  reserved n
  d <- str
  spaces
  return $ t $ d