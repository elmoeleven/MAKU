module ElementsParser where

import Helpers

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Data.Functor.Identity
import qualified Text.Parsec.Prim as P
import qualified Text.ParserCombinators.Parsec.Token as T
import qualified DAST as DAST

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
elements :: Parser DAST.Elements
elements = do
  sP <- getPosition
  reserved "elements"
  _ <- indented sP
  nP <- getPosition
  b <- bullets
  _ <- inline nP
  (a:as) <- antags (map DAST.getBulletName b)
  u <- option Nothing (upgrades (map DAST.getBulletName b) nP)
  return $ DAST.Elements b (DAST.NEList a as) u

bullets :: Parser [DAST.Bullet]
bullets = do
  b <- many bullet
  return $ b

-- parse bullets
bullet :: Parser DAST.Bullet
bullet = do
  sP <- getPosition
  reserved "bullet"
  _ <- indented sP
  nP <- getPosition
  n <- name
  _ <- inline nP
  o <- sscw
  return $ DAST.Bullet n o

sscw :: Parser DAST.SSCW
sscw = do
  sP <- getPosition
  sz <- size
  _ <- inline sP
  shp <- shape
  _ <- inline sP
  clr <- color
  _ <- inline sP
  w <- weight
  return $ DAST.SSCW sz shp clr w

mt :: [String] -> Parser (Maybe DAST.Turret)
mt b = turret "mt" b

mt' :: SourcePos -> [String] -> Parser (Maybe DAST.Turret)
mt' nP b = turret' "mt" b nP

lt' :: SourcePos -> [String] -> Parser (Maybe DAST.Turret)
lt' nP b = turret' "lt" b nP

rt' :: SourcePos -> [String] -> Parser (Maybe DAST.Turret)
rt' nP b = turret' "rt" b nP

turret' :: String -> [String] -> SourcePos -> Parser (Maybe DAST.Turret)
turret' x b nP = try $ do
  _ <- inline nP
  reserved x
  s <- shot b
  case x of
    "mt" -> return $ Just $ DAST.TM s
    "lt" -> return $ Just $ DAST.TL s
    "rt" -> return $ Just $ DAST.TR s

lt :: [String] -> Parser (Maybe DAST.Turret)
lt x = turret "lt" x

rt :: [String] -> Parser (Maybe DAST.Turret)
rt x = turret "rt" x

lb :: [String] -> Parser (Maybe DAST.Turret)
lb x = turret "lb" x

rb :: [String] -> Parser (Maybe DAST.Turret)
rb x = turret "rb" x

turret :: String -> [String] -> Parser (Maybe DAST.Turret)
turret x b = try $ do
  reserved x
  s <- shot b
  case x of
    "mt" -> return $ Just $ DAST.TM s
    "lt" -> return $ Just $ DAST.TL s
    "rt" -> return $ Just $ DAST.TR s
    "lb" -> return $ Just $ DAST.BL s
    "rb" -> return $ Just $ DAST.BR s

noShot :: Parser (Maybe DAST.Shot)
noShot = try $ do
  reserved "none"
  spaces
  return $ Nothing

shot :: [String] -> Parser (Maybe DAST.Shot)
shot b = try $ do
  n <- between quote quote (many alphaNum)
  spaces
  c <- digit'
  spaces
  if elem n b
  then return $ Just $ DAST.Shot n (DAST.ShotType c)
  else bulletNotFoundError n


shotUpgrades :: [String] -> SourcePos -> Parser [DAST.ShotUpgrade]
shotUpgrades x y = try $ do
  a <- many $ shotUpgrade x y
  return $ a


shotUpgrade :: [String] -> SourcePos -> Parser DAST.ShotUpgrade
shotUpgrade b x = try $ do
  _ <- inline x
  reserved "shot"
  n <- between quote quote (many alphaNum)
  spaces
  t <- mt b <|> lt b <|> rt b
  spaces
  return $ DAST.ShotUpgrade n t

-- parse antags
antags :: [String] -> Parser [DAST.Antag]
antags s = do
  a <- many $ antag s
  return $ a

-- parse antag
antag :: [String] -> Parser DAST.Antag
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
  return $ DAST.Antag n o s m

turretType :: [String] -> Parser DAST.TurretType
turretType x = do
  t <- traditional x <|> double x <|> quad x
  spaces
  return $ t

traditional :: [String] -> Parser DAST.TurretType
traditional x = do
  sP <- getPosition
  reserved "traditional"
  _  <- indented sP
  nP <- getPosition
  a  <- mt' nP x
  b  <- option Nothing (lt' nP x)
  c  <- option Nothing (rt' nP x)
  spaces
  return $ DAST.Traditional a b c

double :: [String] -> Parser DAST.TurretType
double x = do
  sP <- getPosition
  reserved "double"
  _  <- indented sP
  nP <- getPosition
  b <- lt' nP x
  c <- rt' nP x
  spaces
  return $ DAST.Double b c

quad :: [String] -> Parser DAST.TurretType
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
  return $ DAST.Quad a b c d

score :: Parser DAST.Nat
score = readNat "score"

msscw :: Parser DAST.MSSCW
msscw = try $ do
  sP <- getPosition
  m  <- movement
  _  <- inline sP
  o  <- sscw
  return $ DAST.MSSCW m o

upgrades :: [String] -> SourcePos -> Parser (Maybe DAST.Upgrades)
upgrades x n = try $ do
  _  <- inline n
  sP <- getPosition
  reserved "upgrades"
  _  <- indented sP
  nP <- getPosition
  s  <- option [] (shotUpgrades x nP)
  sd <- option [] (shields nP)
  b  <- option [] (bombs nP)
  l  <- option [] (lives nP)
  return $ Just $ DAST.Upgrades s sd b l

-- parse shields
shields :: SourcePos -> Parser [DAST.Shield]
shields x = try $ do
  s <- many $ shield x
  return $ s

-- parse shield
shield :: SourcePos -> Parser DAST.Shield
shield x = try $ do
  _ <- inline x
  reserved "shield"
  n <- between quote quote (many alphaNum)
  spaces
  w <- digit'
  spaces
  c <- color'
  return $ DAST.Shield n c w

color' :: Parser DAST.Colour
color' = try $ do
  a <- line
  return $ makeColor $ a

-- parse bombs
bombs :: SourcePos -> Parser [DAST.Bomb]
bombs x = try $ do
  b <- many $ bomb x
  return $ b

-- parse bomb
bomb :: SourcePos -> Parser DAST.Bomb
bomb x = try $ do
  _ <- inline x
  n <- readStr "bomb"
  c <- digit'
  return $ DAST.Bomb n c

lives :: SourcePos -> Parser [DAST.Life]
lives x = try $ do
  l <- many $ life x
  return $ l

life :: SourcePos -> Parser DAST.Life
life x = try $ do
  _ <- inline x
  n <- readStr "life"
  t <- lifeType
  c <- digit'
  return $ DAST.Life n t c

lifeType :: Parser DAST.LifeType
lifeType = try $ do
  s <- options [
    ("add", DAST.Add),
    ("mult", DAST.Mult)]
  spaces
  return $ s

---- parse names
name :: Parser String
name = readStr "name"

-- parse size
size :: Parser DAST.Size
size = try $ do
  reserved "size"
  s <- options [
    ("s", DAST.Small),
    ("m", DAST.Medium),
    ("l", DAST.Large)]
  spaces
  return $ s

-- parse shape
shape :: Parser DAST.Shape
shape = try $ do
  reserved "shape"
  s <- options [
    ("tri", DAST.Triangle),
    ("square", DAST.Square),
    ("rectangle", DAST.Rectangle),
    ("pentagon", DAST.Pentagon),
    ("circle", DAST.Circle)]
  spaces
  return $ s

-- parse movement without movement pattern
movement :: Parser DAST.Movement
movement = try $ do
  sP <- getPosition
  reserved "movement"
  _  <- indented sP
  nP <- getPosition
  p  <- pattern
  _  <- inline nP
  t  <- track
  r  <- option Nothing (rotation nP)
  return $ DAST.Movement p t r

-- parse pattern
pattern :: Parser (Maybe DAST.MovementPattern)
pattern = try $ do
  reserved "pattern"
  p <- options [
    ("x", Just $ DAST.X),
    ("y", Just $ DAST.Y),
    ("panX", Just $ DAST.PanX),
    ("panY", Just $ DAST.PanY),
    ("step", Just $ DAST.Step),
    ("zigzag", Just $ DAST.Zigzag),
    ("spiral", Just $ DAST.Spiral),
    ("wave", Just $ DAST.Wave),
    ("circular", Just $ DAST.Circular),
    ("lshaped", Just $ DAST.LShaped),
    ("none", Nothing)]
  spaces
  return $ p

-- parse weight
weight :: Parser DAST.Nat
weight = readNat "weight"

-- parse movement rotation
rotation :: SourcePos -> Parser (Maybe DAST.RotationData)
rotation nP = try $ do
  _ <- inline nP      -- starting position
  reserved "rotation"
  spaces
  d <- direction
  spaces
  s' <- speed
  return $ Just $ DAST.RotationData d s'

direction :: Parser DAST.Direction
direction = try $ do
  d <- options [
    ("acw", DAST.ACW),
    ("cw", DAST.CW)]
  spaces
  return $ d

speed :: Parser DAST.Speed
speed = try $ do
  s' <- options [
    ("s", DAST.S),
    ("m", DAST.M),
    ("f", DAST.F)]
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

color :: Parser DAST.Colour
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

readNat :: String -> Parser DAST.Nat
readNat n = try $ do
  reserved n
  d <- digit'
  spaces
  return $ d

