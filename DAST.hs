module DAST where

data NEList a = NEList a [a] deriving (Show)

data Game = Game {
  getGrid :: Grid,
  getElements :: Elements,
  getLogic :: Logic
} deriving (Show)

data Logic = Logic {
  getRandoms :: [Random],
  getSingles :: NEList Single,
  getGroups :: NEList Group,
  getLevels :: NEList Level
} deriving (Show)

data Grid = Grid {
  getGridCount :: Nat
} deriving (Show)

data Group = Group {
  getGroupName :: String,
  getGroupUsing :: String, --Antag,
  getGroupLanes :: Nat,
  getGroupCount :: Nat,
  getGroupIP :: IP
} deriving (Show)

data Single = Single {
  getSingleName :: String,
  getSingleUsing :: String, --Antag,
  getSingleIP :: IP
} deriving (Show)

data Random = Random {
  getRandomName :: String,
  getRandomUsing :: String, --Antag,
  getRandomSide  :: Side,
  getRandomCount :: Nat,
  getRandomWait :: Nat
} deriving (Show)

data IP = IP {
  getRow :: Nat,
  getCol :: Nat
} deriving (Show)

data Level = Level {
  getProtag :: Protag,
  getTimeline :: Timeline,
  getWinCondition :: WinCondition
} deriving (Show)

data Timeline = Timeline {
  getTimestamps :: NEList Timestamp
} deriving (Show)

data Timestamp = Timestamp {
  getTimestampType :: Double,
  getBegin :: Nat,
  getWhat :: String,
  getDuration :: Maybe Duration
} deriving (Show)

data Elements = Elements {
  getBullets :: [Bullet],
  getAntags :: NEList Antag,
  getUpgrades :: Maybe Upgrades
} deriving (Show)

data Protag = Protag {
  getProtagColour :: Colour,
  getProtagTurretType  :: TurretType,
  getProtagWeight :: Nat,
  getProtagBombs :: Nat,
  getProtagLives :: LifeCount
} deriving (Show)

data Antag = Antag {
  getAntagName :: String,
  getAntagMSSCW :: MSSCW,
  getAntagScore :: Nat,
  getAntagTurretType :: TurretType
} deriving (Show)

data TurretType = Traditional {
  getTMainTurret  :: Turret,
  getTLeftTurret  :: Turret,
  getTRightTurret  :: Turret
} | Double {
  getDLeftTurret :: Turret,
  getDRightTurret :: Turret
} | Quad {
  getQLeftBottom :: Turret,
  getQRightBottom :: Turret,
  getQLeftTop :: Turret,
  getQRightTop :: Turret
} deriving (Show)


data Upgrades = Upgrades {
  getShotUpgrades :: [ShotUpgrade],
  getShieldUpgrades :: [Shield],
  getBombUpgrades :: [Bomb],
  getLifeUpgrades :: [Life]
} deriving (Show)

data ShotUpgrade = ShotUpgrade {
  getShotUpgradeName :: String,
  getShotTurret :: Turret
} deriving (Show)

data Bullet = Bullet {
  getBulletName :: String,
  getBulletSSCW :: SSCW
} deriving (Show)

data SSCW = SSCW {
  getSize :: Size,
  getShape :: Shape,
  getColour :: Colour,
  getWeight :: Nat
} deriving (Show)

data MSSCW = MSSCW {
  getMovement :: Movement,
  getSSCW :: SSCW
} deriving (Show)

data Shield = Shield {
  getShieldName :: String,
  --getShieldMSCCW :: MSSCW,
  getShieldColour :: Colour,
  getShieldWeight :: Nat
} deriving (Show)

data Shot = Shot {
  getBullet :: String,
  getType   :: ShotType
} deriving (Show)

data Bomb = Bomb {
  getBombName :: String,
  getBombCount :: Nat
} deriving (Show)

data Life = Life {
  getLifeName :: String,
  getLifeType :: LifeType,
  getLifeCount :: Nat
} deriving (Show)

data LifeType = Add | Mult deriving (Show)
data LifeCount = Countable Nat | Infinite deriving (Show)

data Turret = TM Shot | TL Shot | TR Shot | BR Shot | BL Shot deriving (Show)
data WinCondition = Boss String | Score Nat | Time Nat deriving (Show)
data ShotType = ShotType Nat deriving (Show)
data Movement = Movement {
  getPattern :: Maybe MovementPattern,
  getTrack :: Bool,
  getRotation :: Maybe RotationData
} deriving (Show)

data Side = LeftSide | RightSide | TopSide | BottomSide deriving (Show)

data MovementPattern
  = X
  | Y
  | PanY
  | PanX
  | Step
  | Zigzag
  | Spiral
  | Wave
  | Circular
  | LShaped deriving (Show)

data Shape = Triangle | Square | Rectangle | Pentagon | Circle deriving (Show)
data Size = Small | Medium | Large deriving (Show)
data RotationData = RotationData {
  getDirection :: Direction,
  getSpeed :: Speed
} deriving (Show)

data Direction = CW | ACW deriving (Show)
data Speed = VS | S | M | F | VF deriving (Show)

data Colour = Colour {
  unClr :: String
} deriving (Show)

data Duration = Duration Float deriving (Show)

newtype Nat = Nat {
  unNat :: Double
} deriving (Show)

toNat :: Double -> Nat
toNat x
  | x < 0 = error "(-)ve nat"
  | otherwise = Nat x
fromNat :: Nat -> Double
fromNat (Nat x) = x
