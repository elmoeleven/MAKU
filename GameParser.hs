module GameParser where


import Text.ParserCombinators.Parsec
import qualified ElementsParser as NEP
import qualified LogicParser as NLP
import DAST
import Helpers
import qualified Translation as T

main :: IO ()
main = do
  c <- getLine
  e <- readFile c
  case (parse parser "stdin" e) of
    Left  y -> putStrLn "Error: " >> print y
    Right (x:_)  -> putStrLn $ show (T.enit x)
    Right [] -> putStrLn "No Results"

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
