module NewElementsParser where

import Helpers

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Data.Functor.Identity
import qualified Text.Parsec.Prim as P
import qualified Text.ParserCombinators.Parsec.Token as T
import qualified NewDataTypes as DT

-- language definition
def :: LanguageDef g
def = emptyDef
  {
    reservedNames   = [
      "elements",
      "bullet",
      "protag",
      "name",
      "movement",
      "rotation",
      "direction",
      "speed",
      "size",
      "shape",
      "color",
      "shot",
      "antag",
      "pattern",
      "track",
      "weight",
      "upgrades",
      "shield",
      "bomb"
    ]
  }

-- lexer setup
makuLexer :: T.TokenParser s
makuLexer     = T.makeTokenParser def

reserved :: String -> P.ParsecT String u Data.Functor.Identity.Identity ()
reserved = T.reserved makuLexer


--
-- helper parsers
--

-- elements parser
elements :: Parser DT.Elements
elements = do
  sP <- getPosition
  reserved "elements"
  _ <- indented sP
  nP <- getPosition
  b <- bullets
  _ <- inline nP
  (a:as) <- antags (map DT.getBulletName b)
  u <- option Nothing (upgrades (map DT.getBulletName b) nP)
  return $ DT.Elements b (DT.NEList a as) u

bullets :: Parser [DT.Bullet]
bullets = do
  b <- many bullet
  return $ b

-- parse bullets
bullet :: Parser DT.Bullet
bullet = do
  sP <- getPosition
  reserved "bullet"
  _ <- indented sP
  nP <- getPosition
  n <- name
  _ <- inline nP
  o <- sscw
  return $ DT.Bullet n o

sscw :: Parser DT.SSCW
sscw = do
  sP <- getPosition
  sz <- size
  _ <- inline sP
  shp <- shape
  _ <- inline sP
  clr <- color
  _ <- inline sP
  w <- weight
  return $ DT.SSCW sz shp clr w

mt :: [String] -> Parser (Maybe DT.Turret)
mt b = turret "mt" b

mt' :: SourcePos -> [String] -> Parser (Maybe DT.Turret)
mt' nP b = turret' "mt" b nP

lt' :: SourcePos -> [String] -> Parser (Maybe DT.Turret)
lt' nP b = turret' "lt" b nP

rt' :: SourcePos -> [String] -> Parser (Maybe DT.Turret)
rt' nP b = turret' "rt" b nP

turret' :: String -> [String] -> SourcePos -> Parser (Maybe DT.Turret)
turret' x b nP = try $ do
  _ <- inline nP
  reserved x
  s <- shot b
  case x of
    "mt" -> return $ Just $ DT.TM s
    "lt" -> return $ Just $ DT.TL s
    "rt" -> return $ Just $ DT.TR s

lt :: [String] -> Parser (Maybe DT.Turret)
lt x = turret "lt" x

rt :: [String] -> Parser (Maybe DT.Turret)
rt x = turret "rt" x

lb :: [String] -> Parser (Maybe DT.Turret)
lb x = turret "lb" x

rb :: [String] -> Parser (Maybe DT.Turret)
rb x = turret "rb" x

turret :: String -> [String] -> Parser (Maybe DT.Turret)
turret x b = try $ do
  reserved x
  s <- shot b
  case x of
    "mt" -> return $ Just $ DT.TM s
    "lt" -> return $ Just $ DT.TL s
    "rt" -> return $ Just $ DT.TR s
    "lb" -> return $ Just $ DT.BL s
    "rb" -> return $ Just $ DT.BR s

noShot :: Parser (Maybe DT.Shot)
noShot = try $ do
  reserved "none"
  spaces
  return $ Nothing

shot :: [String] -> Parser (Maybe DT.Shot)
shot b = try $ do
  n <- between quote quote (many alphaNum)
  spaces
  c <- digit'
  spaces
  if elem n b
  then return $ Just $ DT.Shot n (DT.ShotType c)
  else bulletNotFoundError n


shotUpgrades :: [String] -> Parser [DT.ShotUpgrade]
shotUpgrades x = try $ do
  a <- many $ shotUpgrade x
  return $ a


shotUpgrade :: [String] -> Parser DT.ShotUpgrade
shotUpgrade b = try $ do
  reserved "shot"
  n <- between quote quote (many alphaNum)
  spaces
  t <- mt b <|> lt b <|> rt b
  spaces
  return $ DT.ShotUpgrade n t

-- parse antags
antags :: [String] -> Parser [DT.Antag]
antags s = do
  a <- many $ antag s
  return $ a

-- parse antag
antag :: [String] -> Parser DT.Antag
antag x = do
  sP <- getPosition
  reserved "antag"
  _  <- indented sP
  nP <- getPosition
  n  <- name
  _  <- inline nP
  o  <- msscw
  _  <- inline nP
  s  <- score
  _  <- inline nP
  m  <- turretType x
  spaces
  return $ DT.Antag n o s m

turretType :: [String] -> Parser DT.TurretType
turretType x = do
  t <- traditional x <|> double x <|> quad x
  spaces
  return $ t

traditional :: [String] -> Parser DT.TurretType
traditional x = do
  sP <- getPosition
  reserved "traditional"
  _  <- indented sP
  nP <- getPosition
  a  <- mt' nP x
  b  <- option Nothing (lt' nP x)
  c  <- option Nothing (rt' nP x)
  spaces
  return $ DT.Traditional a b c

double :: [String] -> Parser DT.TurretType
double x = do
  sP <- getPosition
  reserved "double"
  _  <- indented sP
  nP <- getPosition
  b <- lt' nP x
  c <- rt' nP x
  spaces
  return $ DT.Double b c

quad :: [String] -> Parser DT.TurretType
quad x = do
  sP <- getPosition
  reserved "quad"
  _  <-indented sP
  nP <- getPosition
  a  <- lb x
  _  <- inline nP
  b  <- rb x
  _  <- inline nP
  c  <- lt x
  _  <- inline nP
  d  <- rt x
  spaces
  return $ DT.Quad a b c d

score :: Parser DT.Nat
score = readNat "score"

msscw :: Parser DT.MSSCW
msscw = try $ do
  sP <- getPosition
  m  <- movement
  _  <- inline sP
  o  <- sscw
  return $ DT.MSSCW m o

upgrades :: [String] -> SourcePos -> Parser (Maybe DT.Upgrades)
upgrades x n = try $ do
  _  <- inline n
  sP <- getPosition
  reserved "upgrades"
  _  <- indented sP
  nP <- getPosition
  s  <- shotUpgrades x
  _  <- inline nP
  sd <- shields
  _  <- inline nP
  b  <- bombs
  _  <- inline nP
  l  <- lives
  return $ Just $ DT.Upgrades s sd b l

-- parse shields
shields :: Parser [DT.Shield]
shields = try $ do
  s <- many shield
  return $ s

-- parse shield
shield :: Parser DT.Shield
shield = try $ do
  reserved "shield"
  n <- between quote quote (many alphaNum)
  spaces
  w <- digit'
  spaces
  c <- color'
  return $ DT.Shield n c w

color' :: Parser DT.Colour
color' = try $ do
  a <- line
  return $ makeColor $ a

-- parse bombs
bombs :: Parser [DT.Bomb]
bombs = try $ do
  b <- many bomb
  return $ b

-- parse bomb
bomb :: Parser DT.Bomb
bomb = try $ do
  n <- readStr "bomb"
  c <- digit'
  return $ DT.Bomb n c

lives :: Parser [DT.Life]
lives = try $ do
  l <- many life
  return $ l

life :: Parser DT.Life
life = try $ do
  n <- readStr "life"
  t <- lifeType
  c <- digit'
  return $ DT.Life n t c

lifeType :: Parser DT.LifeType
lifeType = try $ do
  s <- options [
    ("add", DT.Add),
    ("mult", DT.Mult)]
  spaces
  return $ s

---- parse names
name :: Parser String
name = readStr "name"

-- parse size
size :: Parser DT.Size
size = try $ do
  reserved "size"
  s <- options [
    ("s", DT.Small),
    ("m", DT.Medium),
    ("l", DT.Large)]
  spaces
  return $ s

-- parse shape
shape :: Parser DT.Shape
shape = try $ do
  reserved "shape"
  s <- options [
    ("tri", DT.Triangle),
    ("square", DT.Square),
    ("rectangle", DT.Rectangle),
    ("pentagon", DT.Pentagon),
    ("circle", DT.Circle)]
  spaces
  return $ s

-- parse movement without movement pattern
movement :: Parser DT.Movement
movement = try $ do
  sP <- getPosition
  reserved "movement"
  _  <- indented sP
  nP <- getPosition
  p  <- pattern
  _  <- inline nP
  t  <- track
  r  <- option Nothing (rotation nP)
  return $ DT.Movement p t r

-- parse pattern
pattern :: Parser (Maybe DT.MovementPattern)
pattern = try $ do
  reserved "pattern"
  p <- options [
    ("x", Just $ DT.X),
    ("y", Just $ DT.Y),
    ("panX", Just $ DT.PanX),
    ("panY", Just $ DT.PanY),
    ("step", Just $ DT.Step),
    ("zigzag", Just $ DT.Zigzag),
    ("spiral", Just $ DT.Spiral),
    ("wave", Just $ DT.Wave),
    ("circular", Just $ DT.Circular),
    ("lshaped", Just $ DT.LShaped),
    ("none", Nothing)]
  spaces
  return $ p

-- parse weight
weight :: Parser DT.Nat
weight = readNat "weight"

-- parse movement rotation
rotation :: SourcePos -> Parser (Maybe DT.RotationData)
rotation nP = try $ do
  _ <- inline nP      -- starting position
  reserved "rotation"
  spaces
  d <- direction
  spaces
  s' <- speed
  return $ Just $ DT.RotationData d s'

direction :: Parser DT.Direction
direction = try $ do
  d <- options [
    ("acw", DT.ACW),
    ("cw", DT.CW)]
  spaces
  return $ d

speed :: Parser DT.Speed
speed = try $ do
  s' <- options [
    ("s", DT.S),
    ("m", DT.M),
    ("f", DT.F)]
  spaces
  return $ s'

-- parse track
track :: Parser Bool
track = try $ do
  reserved "track"
  t <- options $ [
    ("yes", True),
    ("no", False)]
  spaces
  return $ t

color :: Parser DT.Colour
color = do
  reserved "color"
  c <- line
  return $ makeColor c


readStr :: String -> Parser String
readStr n = try $ do
  reserved n
  s' <- str
  spaces
  return $ s'

readNat :: String -> Parser DT.Nat
readNat n = try $ do
  reserved n
  d <- digit'
  spaces
  return $ d

