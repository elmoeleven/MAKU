{-# OPTIONS_GHC -XFlexibleInstances #-}

-- Module adapted from https://github.com/sseefried/js-good-parts

module Pretty where

import JSAST
import NonEmptyList

------------------------

n :: Double -> JSExpression
n x = JSExpressionLiteral (JSLiteralDouble (JSDouble x))

nf :: Float -> JSExpression
nf x = JSExpressionLiteral (JSLiteralFloat (JSFloat x))

num :: Double -> JSLiteral
num x = (JSLiteralDouble (JSDouble x))

exnm :: String -> JSExpression
exnm x = JSExpressionName $ jsName x

nm :: String -> JSName
nm = jsName

exstr :: String -> JSExpression
exstr x = JSExpressionLiteral $ JSLiteralString $ str x

str :: String -> JSString
str = jsString

var :: JSName -> JSExpression -> JSVarStatement
var a b = JSVarStatement [JSVarDecl a (Just $ b)]

new :: JSExpression -> [JSExpression] -> JSExpression
new a b = JSExpressionNew a (invk b)

self :: JSVarStatement
self = var (nm "self") (exnm "this")

levels :: [JSExpression] -> JSVarStatement
levels s = var (nm "lvls") (array s)

antags :: [JSExpression] -> JSVarStatement
antags s = var (nm "antags") (array s)

groups :: [JSExpression] -> JSVarStatement
groups s = var (nm "groups") (array s)

singles :: [JSExpression] -> JSVarStatement
singles s = var (nm "singles") (array s)

randoms :: [JSExpression] -> JSVarStatement
randoms r = var (nm "randoms") (array r)

upgrades :: JSObjectField -> JSObjectField -> JSObjectField -> JSObjectField -> JSVarStatement
upgrades s d b l = var (nm "upgrades") (object [s,d,b,l])

noUpgrades :: JSVarStatement
noUpgrades = var (nm "upgrades") (object [])

level :: JSObjectField -> JSObjectField -> JSObjectField -> JSExpression
level p t w = object [p,t,w]

infinite :: JSExpression
infinite = n 1000000

countable :: Double -> JSExpression
countable a = n a

protag :: Double -> String -> JSExpression -> Double -> JSExpression -> JSObjectField
protag w c tt b l = fieldWithObject "protag"
  [
    ("weight",    n w),
    ("color",     exstr c),
    ("tt",        tt),
    ("bombs",     n b),
    ("lives",     l)
  ]

grid :: Double -> JSStatement
grid x = statement (nm "Environment") (prop "grid") (invoke' [n x])

makeVar :: String -> [JSObjectField] -> JSVarStatement
makeVar x y = var (nm x) (object y)

antag :: String -> Double -> JSExpression -> JSExpression -> JSExpression -> JSExpression -> JSExpression -> Double -> Double -> JSExpression
antag name weight shape tt pattern track r fireRate score =
  object
  [
    field (nm "name")             (exstr name),
    field (nm "weight")           (n weight),
    field (nm "shape")            (shape),
    field (nm "tt")               (tt),
    field (nm "pattern")          (pattern),
    field (nm "track")            (track),
    field (nm "rotation")         (r),
    field (nm "fireRate")         (n fireRate),
    field (nm "score")            (n score)
  ]

traditional :: JSExpression -> JSExpression -> JSExpression -> JSExpression
traditional a b c = paramInvk "TraditionalTurret" [a, b, c]

double :: JSExpression -> JSExpression -> JSExpression
double a b = paramInvk "DoubleTurret" [a, b]

quad :: JSExpression -> JSExpression -> JSExpression -> JSExpression -> JSExpression
quad a b c d = paramInvk "QuadTurret" [a, b, c, d]

shotUpgrade :: String -> Double -> JSExpression -> JSExpression -> JSExpression
shotUpgrade name t x i =
  object
  [
    field (nm "name")            (exstr name),
    field (nm "turret")          (n t),
    field (nm "shot")            (x),
    field (nm "shape")           (square 3 "red"),
    field (nm "pattern")         (spiral),
    field (nm "equipColor")      (exstr "red"),
    field (nm "initialPosition") (i)
  ]

shotUpgrades :: [JSExpression] -> JSObjectField
shotUpgrades s = field (nm "shots") (array s)

shields :: [JSExpression] -> JSObjectField
shields s = field (nm "shields") (array s)

bombs :: [JSExpression] -> JSObjectField
bombs s = field (nm "bombs") (array s)

lives :: [JSExpression] -> JSObjectField
lives s = field (nm "lives") (array s)

shield :: String -> Double -> JSExpression -> JSExpression
shield name w i =
  object
  [
    field (nm "name")            (exstr name),
    field (nm "shape")           (square 3 "red"),
    field (nm "pattern")         (spiral),
    field (nm "equipColor")      (exstr "red"),
    field (nm "weight")          (n w),
    field (nm "initialPosition") (i)
  ]

initialPosition :: Double -> Double -> ([Char], JSExpression)
initialPosition a b = ("initialPosition", object $ fields [("row", n a),("col", n b)])

ip00 :: ([Char], JSExpression)
ip00 = initialPosition 0 0

group :: String -> String -> Double -> Double -> JSExpression -> JSExpression
group name using lanes count i =
  object
  [
    field (nm "name")            (exstr name),
    field (nm "using")           (exstr using),
    field (nm "lanes")           (n lanes),
    field (nm "count")           (n count),
    field (nm "initialPosition") i
  ]

single :: String -> String -> JSExpression -> JSExpression
single name using iP =
  object
  [
    field (nm "name")            (exstr name),
    field (nm "using")           (exstr using),
    field (nm "initialPosition") iP
  ]

random :: String -> String -> JSExpression -> Double -> Double -> JSExpression
random name using side count wait =
  object
  [
    field (nm "name")  (exstr name),
    field (nm "using") (exstr using),
    field (nm "side")  side,
    field (nm "count") (n count),
    field (nm "wait")  (n wait)
  ]

bomb :: String -> JSExpression -> Double -> JSExpression
bomb name iP c =
  object
  [
    field (nm "name")            (exstr name),
    field (nm "shape")           (square 3 "red"),
    field (nm "pattern")         (zigzag),
    field (nm "initialPosition") iP,
    field (nm "count")           (n c)
  ]

life :: String -> JSExpression -> JSExpression -> Double -> JSExpression
life name t iP c =
  object
  [
    field (nm "name")            (exstr name),
    field (nm "type")            t,
    field (nm "shape")           (square 3 "red"),
    field (nm "pattern")         (zigzag),
    field (nm "initialPosition") iP,
    field (nm "count")           (n c)
  ]

mult :: JSExpression
mult = exstr "mult"

add' :: JSExpression
add' = exstr "add"

leftSide :: JSExpression
leftSide = exstr "left"

rightSide :: JSExpression
rightSide = exstr "right"

topSide :: JSExpression
topSide = exstr "top"

bottomSide :: JSExpression
bottomSide = exstr "bottom"

array :: [JSExpression] -> JSExpression
array = JSExpressionLiteral . JSLiteralArray . JSArrayLiteral

timeline :: [JSExpression] -> JSObjectField
timeline s = fieldWithObject "timeline"
  [
    ("timestamps", (array s))
  ]

scoreWin :: Double -> JSObjectField
scoreWin s = fieldWithObject "winCondition"
  [
    ("score", n s)
  ]

timeWin :: Double -> JSObjectField
timeWin s = fieldWithObject "winCondition"
  [
    ("time", n s)
  ]

bossWin :: String -> JSObjectField
bossWin b = fieldWithObject "winCondition"
  [
    ("boss", exstr b)
  ]

timestamp :: Double -> Double -> String -> Float -> JSExpression
timestamp typ begin what duration =
  paramInvk "Timestamp" [n typ, n begin, exstr what, nf duration]

bullet :: JSExpression -> Double -> JSExpression
bullet shape weight = paramInvk "Bullet" [shape, n weight]

straightShot :: JSExpression -> JSExpression -> Double -> JSExpression
straightShot b polarity w = paramInvk "StraightShot" [b, polarity, n w]

manyShot :: Double -> JSExpression -> JSExpression -> Double -> JSExpression
manyShot x b polarity w = paramInvk "ManyShot" [n x, b, polarity, n w]

noString :: JSExpression
noString = exstr ""

-- need to generate random variable names

verySlowSpeed :: JSExpression
verySlowSpeed = n 1

slowSpeed :: JSExpression
slowSpeed = n 5

mediumSpeed :: JSExpression
mediumSpeed = n 15

fastSpeed :: JSExpression
fastSpeed = n 50

veryFastSpeed :: JSExpression
veryFastSpeed = n 100


circle :: Double -> String -> JSExpression
circle radius color = paramInvk "Circle" [exstr color, n radius]

square :: Double -> String -> JSExpression
square sd color  = paramInvk "Square" [n sd, exstr color]

triangle :: Double -> String -> JSExpression
triangle sL color  = paramInvk "Triangle" [n sL, exstr color]

pentagon :: Double -> String -> JSExpression
pentagon r color = paramInvk "Pentagon" [n r, exstr color]

rectangle :: Double -> Double -> String -> JSExpression
rectangle h w color  = paramInvk "Rectangle" [n h, n w, exstr color]

vertical :: JSExpression
vertical = exnm "Vertical"

horizontal :: JSExpression
horizontal = exnm "Horizontal"

panX :: JSExpression
panX = exnm "PanHorizontal"

panY :: JSExpression
panY = exnm "PanVertical"

step :: JSExpression
step = exnm "Step"

zigzag :: JSExpression
zigzag = exnm "Zigzag"

spiral :: JSExpression
spiral = exnm "Spiral"

wave :: JSExpression
wave = exnm "Wave"

circular :: JSExpression
circular = exnm "Circular"

lShaped :: JSExpression
lShaped = exnm "LShaped"

paramInvk :: String -> [JSExpression] -> JSExpression
paramInvk a b = new (exnm a) b

initPos :: Double -> Double -> JSExpression
initPos a b
  = object [
    field (nm "row") (n a),
    field (nm "col") (n b)
  ]

rotation :: [JSExpression] -> JSExpression
rotation [] = false
rotation [_] = false
rotation (d:s:_)
  = object [
    field (nm "direction") d,
    field (nm "speed") s
  ]

false :: JSExpression
false = exnm "false"

true :: JSExpression
true = exnm "true"

speed :: Double -> JSVarStatement
speed s = var (nm "speed") (n s)

object :: [JSObjectField] -> JSExpression
object = JSExpressionLiteral . JSLiteralObject . JSObjectLiteral

field :: JSName -> JSExpression -> JSObjectField
field a b = JSObjectField (Left $ a) b

fieldWithObject :: String -> [(String, JSExpression)] -> JSObjectField
fieldWithObject nameOfField parms =
  field (nm nameOfField)
  (
    object $ fields parms
  )

fields :: [(String, JSExpression)] -> [JSObjectField]
fields [] = []
fields ((x,y):xs) = field (nm x) (y) : fields xs

statement :: JSName -> JSRefinement -> JSRValue -> JSStatement
statement a b c = JSStatementExpression $ JSESApply (singleton $ JSLValue a [([], b)]) c

functionBody :: [JSVarStatement] -> [JSStatement] -> JSFunctionBody
functionBody = JSFunctionBody

prop :: String -> JSRefinement
prop x = JSProperty (nm x)

invk :: [JSExpression] -> JSInvocation
invk = JSInvocation

invoke' :: [JSExpression] -> JSRValue
invoke' a = JSRVInvoke $ singleton $ invk a

nullInvk :: JSRValue
nullInvk = invoke' $ [exnm ""]

function :: String -> [JSName] -> JSFunctionBody -> JSExpression
function name parms body = JSExpressionLiteral $ JSLiteralFunction $ JSFunctionLiteral (Just $ nm name) parms body

enit :: JSVarStatement -> JSVarStatement -> JSVarStatement -> JSVarStatement -> JSVarStatement -> JSVarStatement -> JSStatement -> JSExpression
enit l a r s g u d = function "init" []
  (
    functionBody
    [
      l,
      a,
      r,
      s,
      g,
      u
    ]
    [
      statement (nm "Environment") (prop "init") (invoke' []),
      d,
      statement (nm "Antagonists") (prop "init") (invoke' [exnm "antags"]),
      statement (nm "Randoms") (prop "init") (invoke' [exnm "randoms"]),
      statement (nm "Singles") (prop "init") (invoke' [exnm "singles"]),
      statement (nm "Groups") (prop "init") (invoke' [exnm "groups"]),
      statement (nm "Upgrades") (prop "init") (invoke' [exnm "upgrades"]),
      statement (nm "Levels") (prop "init") (invoke' [exnm "lvls"]),
      statement (nm "Bullets") (prop "init") (invoke' []),
      statement (nm "Environment") (prop "begin") (invoke' [true])
    ]
  )