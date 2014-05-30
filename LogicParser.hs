module LogicParser where

import Helpers

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Data.Functor.Identity
import qualified Text.Parsec.Prim as P
import qualified Text.ParserCombinators.Parsec.Token as T
import qualified DAST as DAST
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


logic :: [String] -> [String] -> Double -> DAST.Elements -> Parser DAST.Logic
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
  return $ DAST.Logic r s g l

lifeCount :: Parser DAST.LifeCount
lifeCount = try $ do
  l <- lifeNum <|> infinite
  return l

lifeNum :: Parser DAST.LifeCount
lifeNum = try $ do
  n <- readNat "lives"
  return $ DAST.Countable n

infinite :: Parser DAST.LifeCount
infinite = try $ do
  reserved "lives"
  reserved "infinite"
  return DAST.Infinite

bombs :: Parser DAST.Nat
bombs = readNat "bombs"

singles :: [String] -> Double -> Parser (DAST.NEList DAST.Single)
singles a d = try $ do
  (s:ss) <- many $ single a d
  spaces
  return $ DAST.NEList s ss

randoms :: [String] -> Parser [DAST.Random]
randoms a = try $ do
  r <- many $ random a
  spaces
  return r

random :: [String] -> Parser DAST.Random
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
  return $ DAST.Random n u s c w

side :: Parser DAST.Side
side = try $ do
  reserved "side"
  s <- options [
    ("left", DAST.LeftSide),
    ("right", DAST.RightSide),
    ("top", DAST.TopSide),
    ("bottom", DAST.BottomSide)]
  spaces
  return s

wait :: Parser DAST.Nat
wait = readNat "wait"

single :: [String] -> Double -> Parser DAST.Single
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
  return $ DAST.Single n u i


iP :: Double -> Parser DAST.IP
iP d = try $ do
  reserved "iP"
  r <- digit'
  spaces
  c <- digit'
  spaces
  if (DAST.unNat r) < 0
     || (DAST.unNat c) < 0
     || (DAST.unNat r) > d
     || (DAST.unNat c) > d
  then ipInvalidError (show $ DAST.unNat r) (show $ DAST.unNat c)
  else return $ DAST.IP r c

groups :: [String] -> Double -> Parser (DAST.NEList DAST.Group)
groups a d = try $ do
  (g:gs) <- many $ group a d
  spaces
  return $ DAST.NEList g gs

group :: [String] -> Double -> Parser DAST.Group
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
  return $ DAST.Group n u l c i

-- parse protags
protag :: [String] -> Parser (Maybe DAST.Protag)
protag q = try $ do
  sP <- getPosition -- starting position
  reserved "protag"
  _  <- indented sP       -- set indent
  nP <- getPosition -- new position
  if greaterIndent sP nP
  then return Nothing
  else do
    clr <- color
    _   <- inline nP         -- preserve indent
    x   <- mt nP q
    spaces
    y   <- option Nothing (lt nP q)
    z   <- option Nothing (rt nP q)
    _   <- inline nP
    w   <- weight
    _   <- inline nP
    b   <- option (DAST.Nat 0) bombs
    _   <- inline nP
    l   <- lifeCount
    return $ Just $ DAST.Protag clr (DAST.Traditional x y z) w b l

data PW = PW SourcePos DAST.Protag

mt :: SourcePos -> [String] -> Parser DAST.Turret
mt nP b = turret "mt" DAST.TM b nP

lt :: SourcePos -> [String] -> Parser (Maybe DAST.Turret)
lt nP b = optionalTurret "lt" DAST.TL b nP

rt :: SourcePos -> [String] -> Parser (Maybe DAST.Turret)
rt nP b = optionalTurret "rt" DAST.TR b nP

optionalTurret :: String -> (DAST.Shot -> DAST.Turret) -> [String] -> SourcePos -> Parser (Maybe DAST.Turret)
optionalTurret x handler b nP = try $ do
  _ <- inline nP
  reserved x
  s <- shot b
  return $ Just $ handler s

turret :: String -> (DAST.Shot -> DAST.Turret) -> [String] -> SourcePos -> Parser DAST.Turret
turret x handler b nP = try $ do
  _ <- inline nP
  reserved x
  s <- shot b
  return $ handler s


shot :: [String] -> Parser DAST.Shot
shot b = try $ do
  n <- between quote quote (many alphaNum)
  spaces
  c <- digit'
  spaces
  if elem n b
  then return $ DAST.Shot n (DAST.ShotType c)
  else bulletNotFoundError n

levels :: [String] -> ([String],[String],[String],[String]) -> [String] -> Parser (DAST.NEList DAST.Level)
levels b t s = do
  (l:ls) <- many $ level b t s
  spaces
  return $ DAST.NEList l ls

-- NEED TO FIX
level :: [String] -> ([String],[String],[String],[String]) -> [String] -> Parser DAST.Level
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
  return $ DAST.Level p t s

boss :: [String] -> Parser DAST.WinCondition
boss x = do
  reserved "boss"
  d <- str
  spaces
  if elem d x
  then return $ DAST.Boss "boss"
  else singleNotFoundError d

score :: Parser DAST.WinCondition
score = readNatWithType DAST.Score "score"

time :: Parser DAST.WinCondition
time = readNatWithType DAST.Time "time"

timeline :: ([String],[String],[String],[String]) -> Parser (Maybe DAST.Timeline)
timeline x = try $ do
  sP <- getPosition
  reserved "timeline"
  _  <- indented sP
  nP <- getPosition
  if greaterIndent sP nP
  then return Nothing
  else do
    t <- timestamps x
    return $ Just $ DAST.Timeline t

timestamps :: ([String],[String],[String],[String]) -> Parser (DAST.NEList DAST.Timestamp)
timestamps x = try $ do
  (t:ts) <- many $ timestamp x
  return $ DAST.NEList t ts

timestamp :: ([String],[String],[String],[String]) -> Parser DAST.Timestamp
timestamp (r,g,s,u) = try $ do
  reserved "timestamp"
  b <- digit'
  spaces
  n <- str
  spaces
  d <- duration
  if elem n r
  then return $ DAST.Timestamp 3 b n d
  else
    if elem n g
    then return $ DAST.Timestamp 0 b n d
    else
      if elem n s
      then return $ DAST.Timestamp 2 b n d
      else
        if elem n u
        then return $ DAST.Timestamp 1 b n d
        else whatInvalidError n

duration :: Parser (Maybe DAST.Duration)
duration = try $ do
  d <- optionMaybe float
  case d of
    Just x -> return $ Just $ DAST.Duration $ x
    Nothing -> return Nothing

-- adapted from: https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/parsing-floats-with-parsec
float :: Parser Float
float = try $ do
  d <- fmap rd $ many1 digit <++> decimal
  spaces
  return d
    where rd       = read :: String -> Float
          decimal  = option "" $ char '.' <:> (many1 digit)

color :: Parser DAST.Colour
color = try $ do
  reserved "color"
  c <- line
  return $ makeColor c

-- parse weight
weight :: Parser DAST.Nat
weight = readNat "weight"

name :: Parser String
name = readStr "name"

using :: [String] -> Parser String
using a = try $ do
  reserved "using"
  s <- str
  spaces
  if elem s a
  then return s
  else antagNotFoundError s

lanes :: Parser DAST.Nat
lanes = readNat "lanes"

counter :: Parser DAST.Nat
counter = readNat "count"

readStr :: String -> Parser String
readStr n = try $ do
  reserved n
  s <- str
  spaces
  return s

readNat :: String -> Parser DAST.Nat
readNat n = try $ do
  reserved n
  d <- digit'
  spaces
  return d

readNatWithType :: (DAST.Nat -> a) -> String -> Parser a
readNatWithType t n = try $ do
  reserved n
  d <- digit'
  spaces
  return $ t d

readStringWithType :: (String -> a) -> String -> Parser a
readStringWithType t n = try $ do
  reserved n
  d <- str
  spaces
  return $ t d
