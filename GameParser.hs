module GameParser where


import Text.ParserCombinators.Parsec
import qualified NewElementsParser as NEP
import qualified NewLogicParser as NLP
import NewDataTypes
import Helpers
import qualified Translation as T

main :: IO ()
main = do
  e <- readFile "test.maku"
  case (parse parser "stdin" e) of
    Left  y -> putStrLn "Error: " >> print y
    Right (x:_)  -> putStrLn $ show (T.enit x)
    Right [] -> putStrLn "No Results"

parser :: Parser [Game]
parser = do
  spaces
  many1 game

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
