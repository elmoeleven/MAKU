module Language where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as T
import Data.Functor.Identity
import qualified Text.Parsec.Prim as P

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
      "bomb",
      "x",
      "y",
      "panX",
      "panY",
      "step",
      "zigzag",
      "spiral",
      "wave",
      "circular",
      "lshaped",
      "none",
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
      "time",
      "acw",
      "cw",
      "s",
      "m",
      "f",
      "no",
      "yes",
      "tri",
      "square",
      "rectangle",
      "pentagon",
      "circle",
      "add",
      "mult",
      "life",
      "lb",
      "rb",
      "left",
      "right",
      "top",
      "bottom"
    ]
  }

reserved :: String -> P.ParsecT String u Data.Functor.Identity.Identity ()
reserved = T.reserved $ T.makeTokenParser def