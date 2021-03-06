{-# OPTIONS_GHC -XFlexibleInstances #-}

module Translation where

import DAST
import Helpers (toList)

--import DataTypes
import qualified Pretty as P

-- System libraries
import qualified Text.PrettyPrint.Leijen as L
import Data.List
import JSAST

-- FIXME: Code used 0/1 with no good reason.
data Two = Zero | One

groups :: [Group] -> JSVarStatement
groups = P.groups . (map group')

singles :: [Single] -> JSVarStatement
singles = P.singles . (map single)

randoms :: [Random] -> JSVarStatement
randoms = P.randoms . (map random)

levels :: [Bullet] -> [Level] -> JSVarStatement
levels b x = P.levels $ map (level b) x

ssc :: Two -> SSCW -> JSExpression
ssc y x =
  case shp x of
    Triangle -> P.triangle n c
    Square -> P.square n c
    Rectangle -> P.rectangle n n c
    Pentagon -> P.pentagon n c
    Circle -> P.circle n c
  where
    c  = clr x
    n  = size y (getSize x)

shp :: SSCW -> Shape
shp = getShape

clr :: SSCW -> String
clr = unClr . getColour

clr' :: Protag -> String
clr' = unClr . getProtagColour

wght :: SSCW -> Double
wght = unNat . getWeight

size :: Num a => Two -> Size -> a
size Zero Small = 25
size Zero Medium = 35
size Zero Large = 45

size One Small = 3
size One Medium = 4
size One Large = 5

elements' :: Elements -> ([Bullet], JSVarStatement, JSVarStatement)
elements' x = (b, a', u')
  where
    b  = getBullets x
    a' = antags b (toList $ getAntags x)
    u' = upgrades (getUpgrades x) b


logic' :: Logic -> [Bullet] -> (JSVarStatement, JSVarStatement, JSVarStatement, JSVarStatement)
logic' x b = (l', s', g', r')
  where
    r' = randoms $ getRandoms x
    s' = singles $ toList $ getSingles x
    g' = groups $ getGroups x
    l' = levels b (toList $ getLevels x)

enit :: Game -> L.Doc
enit x = L.pretty $ P.enit l' a' r' s' g' u' d'
  where
    d' = P.grid $ grid $ getGrid x
    (b, a', u') = elements' $ getElements x
    (l', s', g', r') = logic' (getLogic x) b

grid :: Grid -> Double
grid (Grid (Nat x)) = x


level :: [Bullet] -> Level -> JSExpression
level b x = P.level p t w
  where
    p = protag (getProtag $ x) b
    t =timeline $ toList $ getTimestamps $ getTimeline $ x
    w = winCondition $ getWinCondition $ x

timeline :: [Timestamp] -> JSObjectField
timeline = P.timeline . (map timestamp)

winCondition :: WinCondition -> JSObjectField
winCondition (Boss x) = P.bossWin x
winCondition (Score (Nat x)) = P.scoreWin x
winCondition (Time (Nat x)) = P.timeWin x

timestamp :: Timestamp -> JSExpression
timestamp x = P.timestamp t' b' w' d'
  where
    t' = getTimestampType x
    b' = unNat $ getBegin x
    w' = getWhat x
    d' = duration $ getDuration x

duration :: Maybe Duration -> Float
duration Nothing = 0
duration (Just (Duration x)) = x

group' :: Group -> JSExpression
group' x = P.group n u' l' c i
  where
    n  = getGroupName x
    u' = getGroupUsing x
    l' = unNat $ getGroupLanes x
    c  = unNat $ getGroupCount x
    i  = initPos $ getGroupIP x

single :: Single -> JSExpression
single x = P.single n u' i
  where
    n  = getSingleName x
    u' = getSingleUsing x
    i  = initPos $ getSingleIP x

random :: Random -> JSExpression
random x = P.random n u' s' c w'
  where
    n  = getRandomName x
    u' = getRandomUsing x
    s' = side $ getRandomSide x
    c  = unNat $ getRandomCount x
    w' = unNat $ getRandomWait x


side :: Side -> JSExpression
side LeftSide = P.leftSide
side RightSide = P.rightSide
side TopSide = P.topSide
side BottomSide = P.bottomSide

initPos :: IP -> JSExpression
initPos (IP r c) = P.initPos (unNat r) (unNat c)

protag :: Protag -> [Bullet] -> JSObjectField
protag x b = P.protag w c tt b' l'
  where
    w  = unNat $ getProtagWeight x
    c  = clr' x
    tt = turretType (getProtagTurretType x) Zero b
    b' = unNat $ getProtagBombs x
    l' = lifeCount $ getProtagLives x

lifeCount :: LifeCount -> JSExpression
lifeCount Infinite = P.infinite
lifeCount (Countable (Nat x)) = P.countable x

antags :: [Bullet] -> [Antag] -> JSVarStatement
antags b x = P.antags $ map (antag b) x

antag :: [Bullet] -> Antag -> JSExpression
antag b x = P.antag n w' h m p t r' 1000.0 s'
  where
    n  = getAntagName $ x
    ((h,w'), (p, t, r')) = msscw $ getAntagMSSCW x
    s' = unNat $ getAntagScore x
    m  = turretType (getAntagTurretType x) One b

turretType :: TurretType -> Two -> [Bullet] -> JSExpression
turretType (Traditional x (Just y) (Just z)) i b = P.traditional m l' r'
  where
    m  = shot (turret x) i b
    l' = shot (turret y) i b
    r' = shot (turret z) i b

turretType (Traditional x (Just y) Nothing) i b = P.traditional m l' P.noString
  where
    m  = shot (turret x) i b
    l' = shot (turret y) i b

turretType (Traditional x Nothing (Just z)) i b = P.traditional m P.noString r'
  where
    m  = shot (turret x) i b
    r' = shot (turret z) i b

turretType (Traditional x Nothing Nothing) i b = P.traditional m P.noString P.noString
  where
    m  = shot (turret x) i b

turretType (Double x y) i b = P.double l' r'
  where
    l' = shot (turret x) i b
    r' = shot (turret y) i b

turretType (Quad w x y z) i b = P.quad a' b' c d'
  where
    a' = shot (turret w) i b
    b' = shot (turret x) i b
    c  = shot (turret y) i b
    d' = shot (turret z) i b


msscw :: MSSCW -> ((JSExpression, Double), (JSExpression, JSExpression, JSExpression))
msscw x = (s',m)
  where
    s' = sscw Zero (getSSCW x)
    m  = movement $ getMovement x

sscw :: Two -> SSCW -> (JSExpression, Double)
sscw y x = (s',w')
  where
    s' = ssc y x
    w' = wght x

movement :: Movement -> (JSExpression, JSExpression, JSExpression)
movement x = (p,t,r')
  where
    p  = pattern $ getPattern x
    t  = boolean $ getTrack x
    r' = rotation $ getRotation x

pattern :: Maybe MovementPattern -> JSExpression
pattern (Just LR) = P.lR
pattern (Just RL) = P.rL
pattern (Just TB) = P.tB
pattern (Just BT) = P.bT
pattern (Just PanX) = P.panX
pattern (Just PanY) = P.panY
pattern (Just Step) = P.step
pattern (Just Zigzag) = P.zigzag
pattern (Just Spiral) = P.spiral
pattern (Just Wave) = P.wave
pattern (Just Circular) = P.circular
pattern (Just LShaped) = P.lShaped
pattern Nothing = P.noString

boolean :: Bool -> JSExpression
boolean False = P.false
boolean True = P.true

rotation :: Maybe RotationData -> JSExpression
rotation Nothing = P.rotation []
rotation (Just x) = P.rotation [d',s']
  where
    d' = direction $ getDirection x
    s' = speed $ getSpeed x

direction :: Direction -> JSExpression
direction x =
  case x of
    ACW -> P.false
    CW  -> P.true

speed :: Speed -> JSExpression
speed x =
  case x of
    VS -> P.verySlowSpeed
    S  -> P.slowSpeed
    M  -> P.mediumSpeed
    F  -> P.fastSpeed
    VF -> P.veryFastSpeed

shot :: Shot -> Two -> [Bullet] -> JSExpression
shot x@(Shot _ t) Zero b =
  let tt = typ t in
  case tt of
  1 -> P.straightShot s' P.true w'
  _ -> P.manyShot tt s' P.true w'
  where
    (s',w') = bulletShape x b

shot x@(Shot _ t) One b =
  let tt = typ t in
  case tt of
  1 -> P.straightShot s' P.false w'
  _ -> P.manyShot tt s' P.false w'
  where
    (s',w') = bulletShape x b

checkBulletExists :: String -> [Bullet] -> Maybe Int
checkBulletExists q b = q `elemIndex` map getBulletName b

bulletShape :: Shot -> [Bullet] -> (JSExpression, Double)
bulletShape x b = handleBullet (checkBulletExists (getBullet x) b) b

handleBullet :: Maybe Int -> [Bullet] -> (JSExpression, Double)
handleBullet (Just i) b = let y = getBulletSSCW $ b !! i in sscw One y
handleBullet Nothing _ = (P.noString, 0.0)

typ :: ShotType -> Double
typ (ShotType t) = fromNat t

upgrades :: Maybe Upgrades -> [Bullet] -> JSVarStatement
upgrades Nothing _ = P.noUpgrades
upgrades (Just x) b = P.upgrades s' d' b' l'
  where
    s' = P.shotUpgrades $ shots b (getShotUpgrades x)
    d' = P.shields $ shields $ getShieldUpgrades x
    b' = P.bombs $ bombs $ getBombUpgrades x
    l' = P.lives $ lives $ getLifeUpgrades x


turret :: Turret -> Shot
turret (TM x) = x
turret (TL x) = x
turret (TR x) = x
turret (BR x) = x
turret (BL x) = x

shots :: [Bullet] -> [ShotUpgrade] -> [JSExpression]
shots b = map (shotUpgrade b)

shotUpgrade :: [Bullet] -> ShotUpgrade -> JSExpression
shotUpgrade b x = P.shotUpgrade n t s' (P.initPos 0 0)
  where
    n  = getShotUpgradeName x
    s' = shot (turret $ getShotTurret x) Zero b
    t  = shotTurret $ getShotTurret x

shotTurret :: Num a => Turret -> a
shotTurret (TM _) = 0
shotTurret (TL _) = 1
shotTurret (TR _) = 2
shotTurret (BR _) = 3
shotTurret (BL _) = 4

lives :: [Life] -> [JSExpression]
lives = map life

life :: Life -> JSExpression
life x = P.life n t (P.initPos 0 0) c
  where
    n = getLifeName x
    t = lifeType $ getLifeType x
    c = unNat $ getLifeCount x


lifeType :: LifeType -> JSExpression
lifeType Add = P.add'
lifeType Mult = P.mult

shields :: [Shield] -> [JSExpression]
shields = map shield

shield :: Shield -> JSExpression
shield x = P.shield n w (P.initPos 0 0)
  where
    n = getShieldName x
  --, c <- unClr $ getShieldColour x
    w = unNat $ getShieldWeight x


bombs :: [Bomb] -> [JSExpression]
bombs = map bomb

bomb :: Bomb -> JSExpression
bomb x = P.bomb n (P.initPos 0 0) c
  where
    n = getBombName x
    c = unNat $ getBombCount x
