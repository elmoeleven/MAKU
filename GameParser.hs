module GameParser where

import System.Process
import Text.ParserCombinators.Parsec
import qualified ElementsParser as NEP
import qualified LogicParser as NLP
import DAST
import Helpers
import qualified Translation as T
import qualified Data.ByteString.Lazy as B

gen :: String -> IO ()
gen f = do
  e <- readFile f
  case (parse parser "stdin" e) of
    Left  y -> putStrLn "Error: " >> print y
    Right (x:_)  -> output x
    Right [] -> putStrLn "No Results"

output :: Game -> IO()
output x = do
  contents <- readFile "engine.js"
  writeFile "maku.js" (contents ++ "\n" ++ show (T.enit x))
  runCommand "uglifyjs maku.js -o maku.min.js"
  putStrLn $ "maku.js generated!"
  --putStrLn $ show (T.enit x)

parser :: Parser [Game]
parser = do
  spaces
  ls <- many1 game
  return $ ls

game :: Parser Game
game = do
  sP <- getPosition
  d <- grid
  _ <- inline sP
  e <- NEP.elements
  l <- NLP.logic (bulletNames e) (antagNames e) (unNat $ getGridCount d) e
  return $ Game d e l

grid :: Parser Grid
grid = NLP.readNatWithType Grid "grid"
